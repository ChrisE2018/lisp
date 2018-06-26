/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

public class Applicable
{
    private static Assignable assignable = new Assignable ();

    /**
     * Determine if this method overload is applicable to the provided arguments. This method is
     * used when the actual arguments are available.
     *
     * @param arguments The arguments that will be used to invoke the method.
     * @return True if it is applicable.
     */
    public boolean applicable (final Method method, final List<Object> arguments)
    {
	final int actualArgCount = arguments.size ();
	final int minimumArgCount = method.getParameterCount () + (method.isVarArgs () ? -1 : 0);
	if (actualArgCount < minimumArgCount)
	{
	    return false;
	}
	if (actualArgCount > minimumArgCount)
	{
	    if (!method.isVarArgs ())
	    {
		return false;
	    }
	}
	// Additional filters here.
	final Class<?>[] types = method.getParameterTypes ();
	for (int i = 0; i < minimumArgCount; i++)
	{
	    final Class<?> argType = types[i]; // What is needed
	    final Object arg = arguments.get (i);
	    if (!assignable.isAssignableFrom (argType, arg))
	    {
		return false;
	    }
	}
	if (actualArgCount > minimumArgCount && method.isVarArgs ())
	{
	    final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	    for (int i = minimumArgCount; i < arguments.size (); i++)
	    {
		final Object arg = arguments.get (i);
		if (!assignable.isAssignableFrom (argType, arg))
		{
		    return false;
		}
	    }
	}
	return true;
    }

    /**
     * Determine if this method overload is applicable to the provided arguments. This method is
     * used when the actual arguments are available.
     *
     * @param arguments The arguments that will be used to invoke the method.
     * @return True if it is applicable.
     */
    public boolean applicable (final Constructor<?> method, final Object[] arguments)
    {
	final int actualArgCount = arguments.length;
	final int minimumArgCount = method.getParameterCount () + (method.isVarArgs () ? -1 : 0);
	if (actualArgCount < minimumArgCount)
	{
	    return false;
	}
	if (actualArgCount > minimumArgCount)
	{
	    if (!method.isVarArgs ())
	    {
		return false;
	    }
	}
	// Additional filters here.
	final Class<?>[] types = method.getParameterTypes ();
	for (int i = 0; i < minimumArgCount; i++)
	{
	    final Class<?> argType = types[i]; // What is needed
	    final Object arg = arguments[i];
	    if (!assignable.isAssignableFrom (argType, arg))
	    {
		return false;
	    }
	}
	if (actualArgCount > minimumArgCount && method.isVarArgs ())
	{
	    final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	    for (int i = minimumArgCount; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		if (!assignable.isAssignableFrom (argType, arg))
		{
		    return false;
		}
	    }
	}
	return true;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
