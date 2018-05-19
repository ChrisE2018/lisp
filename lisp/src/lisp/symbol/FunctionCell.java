
package lisp.symbol;

import java.lang.reflect.Method;
import java.util.*;
import java.util.Map.Entry;

import lisp.*;
import lisp.eval.*;

/** Base class of all function cells. */
public abstract class FunctionCell implements Describer
{
    class ObjectMethod
    {
	Object object;
	Method method;

	ObjectMethod (final Object object, final Method method)
	{
	    this.object = object;
	    this.method = method;
	}

	String getName ()
	{
	    return method.getName ();
	}

	boolean isVarArgs ()
	{
	    return method.isVarArgs ();
	}

	Class<?>[] getParameterTypes ()
	{
	    return method.getParameterTypes ();
	}

	@Override
	public String toString ()
	{
	    final StringBuilder buffer = new StringBuilder ();
	    buffer.append ("#<");
	    buffer.append (getClass ().getSimpleName ());
	    buffer.append (" ");
	    buffer.append (object);
	    buffer.append (" ");
	    buffer.append (method);
	    buffer.append (">");
	    return buffer.toString ();
	}
    }

    /** The symbol this function cell is attached to. */
    private final Symbol symbol;

    private ObjectMethod[] overloads = new ObjectMethod[0];

    abstract public Object eval (final Interpreter interpreter, final List<?> form) throws Exception;

    abstract public void overload (DefineLisp a, Object obj, Method method);

    FunctionCell (final Symbol symbol)
    {
	this.symbol = symbol;
    }

    public Symbol getFunctionName ()
    {
	return symbol;
    }

    /**
     * Get the fixed arg method to apply to n arguments. Do not use this for varArgs functions.
     *
     * @param n
     * @return
     */
    ObjectMethod getOverload (final int n)
    {
	if (overloads.length > n)
	{
	    return overloads[n];
	}
	return null;
    }

    /**
     * Check an array of overloaded methods for ambiguity. Create an array to retrieve the correct
     * method based on the number of arguments in the call.
     */
    void makeOverloadMap (final ObjectMethod[] methods)
    {
	final Map<Integer, ObjectMethod> selector = getOverloadSelector (methods);
	int count = 0;
	for (final int key : selector.keySet ())
	{
	    if (key > count)
	    {
		count = key;
	    }
	}
	final ObjectMethod[] result = new ObjectMethod[count + 1];
	for (final Entry<Integer, ObjectMethod> entry : selector.entrySet ())
	{
	    result[entry.getKey ()] = entry.getValue ();
	}
	overloads = result;
    }

    Map<Integer, ObjectMethod> getOverloadSelector (final ObjectMethod[] methods)
    {
	final Map<Integer, ObjectMethod> selector = new HashMap<Integer, ObjectMethod> ();
	for (final ObjectMethod method : methods)
	{
	    final int c = getMethodSelectorCount (method.method);
	    if (method.isVarArgs ())
	    {
		for (final int key : selector.keySet ())
		{
		    if (key >= c)
		    {
			throw new IllegalArgumentException ("Overloaded method is ambiguous with " + c + " or more arguments");

		    }
		}
	    }
	    if (selector.containsKey (c))
	    {
		throw new IllegalArgumentException (
		        "Overloaded method " + method.getName () + " is ambiguous with " + c + " arguments");
	    }
	    selector.put (c, method);
	}
	return selector;
    }

    int getMethodSelectorCount (final Method method)
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	if (method.isVarArgs ())
	{
	    final int c = parameters.length - 1;
	    return c;
	}
	else
	{
	    final int c = parameters.length;
	    return c;
	}
    }

    /**
     * Select a method that is application for a call.
     *
     * @param methods The set of overloaded methods.
     * @param argCount The argument count of this call.
     * @return The selected method.
     */
    Method selectMethod (final Method[] methods, final int argCount)
    {
	for (final Method method : methods)
	{
	    final Class<?>[] parameters = method.getParameterTypes ();
	    if (method.isVarArgs ())
	    {
		if (argCount >= parameters.length - 1)
		{
		    return method;
		}
	    }
	    else if (argCount == parameters.length)
	    {
		return method;
	    }
	}
	throw new IllegalArgumentException ("No applicable method");
    }

    /** Select a method using the overloads table. */
    public ObjectMethod selectMethod (final int argCount)
    {
	ObjectMethod result = null;
	if (argCount < overloads.length)
	{
	    result = overloads[argCount];
	}
	else
	{
	    final int n = overloads.length - 1;
	    result = overloads[n];
	    if (!result.isVarArgs ())
	    {
		result = null;
	    }
	}
	if (result == null)
	{
	    throw new IllegalArgumentException ("No applicable method");
	}
	return result;
    }

    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target
     * @return
     */
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	for (final ObjectMethod method : overloads)
	{
	    result.put ("Overload", method);
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (symbol);
	buffer.append (">");
	return buffer.toString ();
    }
}
