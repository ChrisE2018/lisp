
package lisp;

import java.lang.reflect.*;

public class SpecialFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;

    public SpecialFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
    }

    @Override
    public Lisp eval (final Interpreter interpreter, final LispList form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Lisp result = (Lisp)method.invoke (obj, form);
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (method);
	buffer.append (">");
	return buffer.toString ();
    }
}
