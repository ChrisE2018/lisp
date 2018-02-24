
package lisp;

import java.lang.reflect.Method;

public class MacroFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;

    public MacroFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
    }

    @Override
    public Lisp eval (final Interpreter interpreter, final LispList form) throws Exception
    {
	final Lisp expanded = (Lisp)method.invoke (obj, form);
	final Lisp result = interpreter.eval (expanded);
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
