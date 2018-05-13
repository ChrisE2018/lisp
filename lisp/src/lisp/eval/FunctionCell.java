
package lisp.eval;

import java.lang.reflect.Method;
import java.util.*;

/** Base class of all function cells. */
public abstract class FunctionCell
{
    abstract public Object eval (final Interpreter interpreter, final List<?> form) throws Exception;

    abstract public void overload (DefineLisp a, Method method);

    /** Check an array of overloaded methods to for ambiguity. */
    void makeOverloadMap (final Method[] newMethods)
    {
	final Map<Integer, Method> selector = new HashMap<Integer, Method> ();
	for (final Method method : newMethods)
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
		    throw new IllegalArgumentException ("Overloaded method is ambiguous with " + c + " arguments");
		}
		selector.put (c, method);
	    }
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
