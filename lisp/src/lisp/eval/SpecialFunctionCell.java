
package lisp.eval;

import java.lang.reflect.Method;
import java.util.List;

public class SpecialFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;
    private final Class<?>[] parameters;
    private final boolean isVarArgs;

    public SpecialFunctionCell (final Object obj, final Method method)
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
	    // Number of parameters excluding the interpreter
	    final int paramsLengthActual = parameters.length - 1;
	    if (form.size () < paramsLengthActual)
	    {
		throw new IllegalArgumentException (
		        "Invalid argument count " + (form.size () - 1) + " require at least " + (parameters.length - 1));
	    }
	    final Object[] arguments = new Object[parameters.length];
	    arguments[0] = interpreter;
	    for (int i = 1; i < paramsLengthActual; i++)
	    {
		arguments[i] = form.get (i);
	    }
	    final int count = form.size () - paramsLengthActual;
	    final Object[] args = new Object[count];
	    for (int i = 0; i < count; i++)
	    {
		args[i] = form.get (paramsLengthActual + i);
	    }
	    arguments[parameters.length - 1] = args;
	    return method.invoke (obj, arguments);
	}
	else
	{
	    // Form includes an extra element for the function name
	    // parameters includes an extra element for the interpreter
	    if (form.size () != parameters.length)
	    {
		throw new IllegalArgumentException (
		        "Invalid argument count " + (form.size () - 1) + " expecting " + (parameters.length - 1));
	    }
	    final Object[] arguments = new Object[parameters.length];
	    arguments[0] = interpreter;
	    for (int i = 1; i < parameters.length; i++)
	    {
		arguments[i] = form.get (i);
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
