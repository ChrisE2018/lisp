/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

import org.objectweb.asm.tree.ClassNode;

import lisp.Symbol;
import lisp.eval.LexicalContext;

public class DefaultFunctionCell extends FunctionCell
{
    public DefaultFunctionCell (final Symbol symbol, final boolean allowRedefinition)
    {
	super (symbol, allowRedefinition);
    }

    @Override
    public Object eval (final LexicalContext context, final List<? extends Object> form) throws Exception
    {
	throw new UnsupportedOperationException ("Can't eval default function definitions");
    }

    @Override
    public Object apply (final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object target = arguments.get (0);
	final String name = getFunctionName ().getName ();
	return apply (target, name, arguments);
    }

    private Object apply (final Object target, final String name, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final int argCount = arguments.size () - 1;
	final Object[] args = new Object[argCount];
	for (int i = 0; i < args.length; i++)
	{
	    args[i] = arguments.get (i + 1);
	}
	// final Object[] args = Arrays.copyOfRange (arguments, 1, arguments.length);
	for (Class<?> cls = target.getClass (); cls != null; cls = cls.getSuperclass ())
	{
	    for (final Method method : cls.getDeclaredMethods ())
	    {
		if (method.getName ().equals (name))
		{
		    // FIXME handle VarArgs
		    if (method.getParameterCount () == argCount)
		    {
			if (canInvoke (method, args))
			{
			    return method.invoke (target, args);
			}
		    }
		}
	    }
	}
	return null;
    }

    private boolean canInvoke (final Method method, final Object[] arguments)
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	for (int i = 0; i < parameters.length; i++)
	{
	    if (!parameters[i].isAssignableFrom (arguments[i].getClass ()))
	    {
		return false;
	    }
	}
	return true;
    }

    @Override
    public void overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
    {
	throw new UnsupportedOperationException ("Can't overload default function definitions");
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
