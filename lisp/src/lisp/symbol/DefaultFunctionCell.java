
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;

import lisp.Symbol;
import lisp.eval.LexicalContext;

public class DefaultFunctionCell extends FunctionCell
{

    public DefaultFunctionCell (final Symbol symbol, final boolean allowRedefinition)
    {
	super (symbol, allowRedefinition);
    }

    @Override
    public Object eval (final LexicalContext context, final List<?> form) throws Exception
    {
	return null;
    }

    @Override
    public Object apply (final Object... arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object target = arguments[0];
	final String name = getFunctionName ().getName ();
	return apply (target, name, arguments);
    }

    private Object apply (final Object target, final String name, final Object[] arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object[] args = Arrays.copyOfRange (arguments, 1, arguments.length);
	for (Class<?> cls = target.getClass (); cls != null; cls = cls.getSuperclass ())
	{
	    for (final Method method : cls.getDeclaredMethods ())
	    {
		if (method.getName ().equals (name))
		{
		    if (method.getParameterCount () == arguments.length - 1)
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
    public void overload (final Object obj, final Method method, final String documentation)
    {
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
