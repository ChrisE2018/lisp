
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;

import lisp.*;
import lisp.eval.LexicalContext;

/** Base class of all function cells. */
public abstract class FunctionCell implements Describer
{
    class ObjectMethod implements Describer
    {
	final Object object;
	final Method method;
	final String documentation;

	ObjectMethod (final Object object, final Method method, final String documentation)
	{
	    this.object = object;
	    this.method = method;
	    this.documentation = documentation;
	}

	private String getName ()
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
	public Map<String, Object> getDescriberValues (final Object target)
	{
	    final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	    result.put ("Object", object);
	    result.put ("Method", method);
	    return result;
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

    private final boolean allowRedefinition;

    private ObjectMethod[] overloads = new ObjectMethod[0];

    abstract public Object eval (final LexicalContext context, final List<?> form) throws Exception;

    abstract public void overload (Object obj, Method method, String documentation);

    FunctionCell (final Symbol symbol, final boolean allowRedefinition)
    {
	this.symbol = symbol;
	this.allowRedefinition = allowRedefinition;
    }

    public Symbol getFunctionName ()
    {
	return symbol;
    }

    public boolean isAllowRedefinition ()
    {
	return allowRedefinition;
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
	    throw new IllegalArgumentException ("No applicable method calling " + symbol + " with " + argCount + " arguments");
	}
	return result;
    }

    /**
     * @param arguments
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws Exception
     */
    @SuppressWarnings ("unused")
    public Object apply (final Object... arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, Exception
    {
	throw new UnsupportedOperationException ();
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
