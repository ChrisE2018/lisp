
package lisp;

import java.lang.reflect.Method;
import java.util.*;

public class StandardFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;

    public StandardFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
    }

    @Override
    public Lisp eval (final Interpreter interpreter, final LispList form) throws Exception
    {
	final List<Lisp> arguments = new ArrayList<Lisp> ();
	for (int i = 1; i < form.size (); i++)
	{
	    final Lisp f = form.get (i);
	    arguments.add (interpreter.eval (f));
	}
	return (Lisp)method.invoke (obj, arguments);
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
