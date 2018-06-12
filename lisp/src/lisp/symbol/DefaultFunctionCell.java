/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

import org.objectweb.asm.tree.ClassNode;

import lisp.Symbol;
import lisp.eval.*;

public class DefaultFunctionCell extends FunctionCell
{
    private static Applicable applicable = new Applicable ();
    private static Assignable assignable = new Assignable ();
    private static Invoke invoke = new Invoke ();

    public DefaultFunctionCell (final Symbol symbol, final boolean allowRedefinition)
    {
	super (symbol, allowRedefinition);
    }

    @Override
    public void overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
    {
	throw new UnsupportedOperationException ("Can't overload default function definitions");
    }

    @Override
    public Object eval (final LexicalContext context, final List<? extends Object> form) throws Exception
    {
	throw new UnsupportedOperationException ("Can't eval default function definitions");
    }

    /**
     * Find a method that can be applied to the arguments.
     *
     * @param arguments Arguments to a function call. The first argument is taken as the target
     *            object to invoke upon. The rest are passed as method arguments. Method selection
     *            is based on argument types and inherits from superclasses of the target object.
     */
    @Override
    public Object apply (final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object target = arguments.get (0);
	final String name = getFunctionName ().getName ();
	final List<Object> args = arguments.subList (1, arguments.size ());
	for (Class<?> cls = target.getClass (); cls != null; cls = cls.getSuperclass ())
	{
	    Method selectedMethod = null;
	    for (final Method method : cls.getDeclaredMethods ())
	    {
		if (method.getName ().equals (name))
		{
		    if (applicable.applicable (method, args))
		    {
			if (selectedMethod == null)
			{
			    selectedMethod = method;
			}
			else if (isBetterThan (selectedMethod, method))
			{
			    // Ignore
			}
			else if (isBetterThan (method, selectedMethod))
			{
			    selectedMethod = method;
			}
			else
			{
			    final StringBuilder buffer = new StringBuilder ();
			    buffer.append ("Ambiguous ");
			    buffer.append (name);
			    buffer.append (" method selection for ");
			    buffer.append (args);
			    buffer.append (". Both ");
			    buffer.append (selectedMethod);
			    buffer.append (" and ");
			    buffer.append (method);
			    buffer.append (" apply.");
			    throw new IllegalArgumentException (buffer.toString ());
			}
		    }
		}
	    }
	    if (selectedMethod != null)
	    {
		return invoke.apply (selectedMethod, target, args);
	    }
	}
	return null;
    }

    /**
     * Determine if this method overload is better than another viable candidate.
     *
     * @param otherMethod Another overload option that is known to be applicable to an argument set.
     * @return True if this method overload should be used in preference to the other overload.
     */
    private boolean isBetterThan (final Method method, final Method otherMethod)
    {
	final Class<?>[] myTypes = method.getParameterTypes ();
	final Class<?>[] otherTypes = otherMethod.getParameterTypes ();
	for (int i = 0; i < myTypes.length; i++)
	{
	    final Class<?> myType = myTypes[i];
	    final Class<?> otherType = otherTypes[i];
	    if (!assignable.isAssignableFrom (myType, otherType))
	    {
		return true;
	    }
	}
	return false;
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
