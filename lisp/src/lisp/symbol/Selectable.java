/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.Method;
import java.util.List;

/**
 * @author cre
 */
public class Selectable
{
    private static Assignable assignable = new Assignable ();

    /**
     * Determine if this method overload is applicable to arguments of the specified classes. This
     * method is used when the argument types are known but the actual values are not known, for
     * example during compilation.
     *
     * @param arguments
     * @return
     */
    public boolean isSelectable (final Method method, final List<Class<?>> arguments)
    {
	return isSelectable (method.getParameterTypes (), method.isVarArgs (), arguments);
	// final int actualArgCount = arguments.size ();
	// final int minimumArgCount = method.getParameterCount () + (method.isVarArgs () ? -1 : 0);
	// if (actualArgCount < minimumArgCount)
	// {
	// return false;
	// }
	// if (actualArgCount > minimumArgCount)
	// {
	// if (!method.isVarArgs ())
	// {
	// return false;
	// }
	// }
	// // Additional filters here.
	// final Class<?>[] types = method.getParameterTypes ();
	// for (int i = 0; i < minimumArgCount; i++)
	// {
	// final Class<?> argType = types[i]; // What is needed
	// final Class<?> argClass = arguments.get (i);
	// if (!assignable.isAssignableFrom (argType, argClass))
	// {
	// return false;
	// }
	// }
	// if (actualArgCount > minimumArgCount && method.isVarArgs ())
	// {
	// final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	// for (int i = minimumArgCount; i < arguments.size (); i++)
	// {
	// final Class<?> argClass = arguments.get (i);
	// if (!assignable.isAssignableFrom (argType, argClass))
	// {
	// return false;
	// }
	// }
	// }
	// return true;
    }

    public boolean isSelectable (final Class<?>[] types, final boolean isVarArgs, final List<Class<?>> arguments)
    {
	final int actualArgCount = arguments.size ();
	final int minimumArgCount = types.length + (isVarArgs ? -1 : 0);
	if (actualArgCount < minimumArgCount)
	{
	    return false;
	}
	if (actualArgCount > minimumArgCount)
	{
	    if (!isVarArgs)
	    {
		return false;
	    }
	}
	// Additional filters here.
	for (int i = 0; i < minimumArgCount; i++)
	{
	    final Class<?> argType = types[i]; // What is needed
	    final Class<?> argClass = arguments.get (i);
	    if (!assignable.isAssignableFrom (argType, argClass))
	    {
		return false;
	    }
	}
	if (actualArgCount > minimumArgCount && isVarArgs)
	{
	    final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	    for (int i = minimumArgCount; i < arguments.size (); i++)
	    {
		final Class<?> argClass = arguments.get (i);
		if (!assignable.isAssignableFrom (argType, argClass))
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
