
package lisp.eval;

import java.lang.reflect.*;
import java.util.*;

public class SpecialFunctionCell extends FunctionCell
{
    private final Object obj;
    private Method[] methods;

    public SpecialFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	methods = new Method[]
	    {method};
	makeOverloadMap (methods);
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
	// Form size is one extra due to the function name &
	// Number of arguments is one extra due to the interpreter argument.
	final Method method = selectMethod (form.size ());
	if (method.isVarArgs ())
	{
	    return applyVarArgs (interpreter, method, form);
	}
	else
	{
	    return applyFixedArgs (interpreter, method, form);
	}
    }

    private Object applyVarArgs (final Interpreter interpreter, final Method method, final List<?> form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	// Number of parameters excluding the interpreter
	final int paramsLengthActual = parameters.length - 1;
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

    private Object applyFixedArgs (final Interpreter interpreter, final Method method, final List<?> form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	// Form includes an extra element for the function name
	// parameters includes an extra element for the interpreter
	final Object[] arguments = new Object[parameters.length];
	arguments[0] = interpreter;
	for (int i = 1; i < parameters.length; i++)
	{
	    arguments[i] = form.get (i);
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
