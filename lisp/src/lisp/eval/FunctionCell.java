
package lisp.eval;

import java.lang.reflect.Method;
import java.util.*;
import java.util.Map.Entry;

import lisp.Describer;

/** Base class of all function cells. */
public abstract class FunctionCell implements Describer
{
    private Method[] overloads = new Method[0];

    abstract public Object eval (final Interpreter interpreter, final List<?> form) throws Exception;

    abstract public void overload (DefineLisp a, Method method);

    /** Check an array of overloaded methods to for ambiguity. */
    void makeOverloadMap (final Method[] methods)
    {
	final Map<Integer, Method> selector = getOverloadSelector (methods);
	int count = 0;
	for (final int key : selector.keySet ())
	{
	    if (key > count)
	    {
		count = key;
	    }
	}
	final Method[] result = new Method[count + 1];
	for (final Entry<Integer, Method> entry : selector.entrySet ())
	{
	    result[entry.getKey ()] = entry.getValue ();
	}
	overloads = result;
    }

    Map<Integer, Method> getOverloadSelector (final Method[] methods)
    {
	final Map<Integer, Method> selector = new HashMap<Integer, Method> ();
	for (final Method method : methods)
	{
	    final Class<?>[] parameters = method.getParameterTypes ();
	    if (method.isVarArgs ())
	    {
		final int c = parameters.length - 1;
		for (final int key : selector.keySet ())
		{
		    if (key >= c)
		    {
			throw new IllegalArgumentException ("Overloaded method is ambiguous with " + c + " or more arguments");

		    }
		}
		selector.put (c, method);
	    }
	    else
	    {
		final int c = parameters.length;
		if (selector.containsKey (c))
		{
		    throw new IllegalArgumentException (
		            "Overloaded method " + method.getName () + " is ambiguous with " + c + " arguments");
		}
		selector.put (c, method);
	    }
	}
	return selector;
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
    Method selectMethod (final int argCount)
    {
	Method result = null;
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
	for (final Method method : overloads)
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
	buffer.append (">");
	return buffer.toString ();
    }
}
