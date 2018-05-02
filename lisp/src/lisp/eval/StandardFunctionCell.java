
package lisp.eval;

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
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final List<Object> arguments = new ArrayList<Object> ();
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments.add (interpreter.eval (f));
	}
	return method.invoke (obj, arguments);
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
