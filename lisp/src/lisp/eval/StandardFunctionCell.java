
package lisp.eval;

import java.lang.reflect.Method;
import java.util.*;

public class StandardFunctionCell extends FunctionCell
{
    private final Object obj;
    private Method[] methods;

    public StandardFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	methods = new Method[]
	    {method};
    }

    @Override
    public void overload (final DefineLisp a, final Method method)
    {
	final Method[] newMethods = Arrays.copyOf (methods, methods.length + 1, Method[].class);
	newMethods[methods.length] = method;
	// Scan methods and determine if there are possible ambiguous ones
	makeOverloadMap (newMethods);
	methods = newMethods;
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final Method method = selectMethod (methods, form.size () - 1);
	if (method.isVarArgs ())
	{
	    return applyVarArgs (interpreter, method, form);
	}
	else
	{
	    return applyFixedArgs (interpreter, method, form);
	}
    }

    private Object applyVarArgs (final Interpreter interpreter, final Method method, final List<?> form) throws Exception
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] arguments = new Object[parameters.length];
	// Collect the required arguments
	for (int i = 1; i < parameters.length; i++)
	{
	    final Object f = form.get (i);
	    arguments[i - 1] = interpreter.eval (f);
	}
	// Collect the optional arguments
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

    private Object applyFixedArgs (final Interpreter interpreter, final Method method, final List<?> form) throws Exception
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] arguments = new Object[parameters.length];
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments[i - 1] = interpreter.eval (f);
	}
	return method.invoke (obj, arguments);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	for (final Method m : methods)
	{
	    buffer.append (" ");
	    buffer.append (m);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
