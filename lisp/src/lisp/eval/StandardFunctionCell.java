
package lisp.eval;

import java.lang.reflect.Method;
import java.util.List;

public class StandardFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;
    private final Class<?>[] parameters;
    private final boolean isVarArgs;

    public StandardFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
	parameters = method.getParameterTypes ();
	isVarArgs = method.isVarArgs ();
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	if (isVarArgs)
	{
	    if (form.size () < parameters.length)
	    {
		throw new IllegalArgumentException (
		        "Invalid argument count " + (form.size () - 1) + " require at least " + (parameters.length - 1));
	    }
	    final Object[] arguments = new Object[parameters.length];
	    for (int i = 1; i < parameters.length; i++)
	    {
		final Object f = form.get (i);
		arguments[i - 1] = interpreter.eval (f);
	    }
	    final int count = form.size () - parameters.length;
	    final Object[] args = new Object[count];
	    for (int i = 0; i < count; i++)
	    {

		final Object f = form.get (parameters.length + i);
		args[i] = interpreter.eval (f);
	    }
	    arguments[parameters.length - 1] = args;
	    return method.invoke (obj, arguments);
	}
	else
	{
	    if (form.size () != parameters.length + 1)
	    {
		throw new IllegalArgumentException (
		        "Invalid argument count " + (form.size () - 1) + " expecting " + parameters.length);
	    }
	    final Object[] arguments = new Object[parameters.length];
	    for (int i = 1; i < form.size (); i++)
	    {
		final Object f = form.get (i);
		arguments[i - 1] = interpreter.eval (f);
	    }
	    return method.invoke (obj, arguments);
	}
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
