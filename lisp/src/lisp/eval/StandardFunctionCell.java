
package lisp.eval;

import java.lang.reflect.*;
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
	final Method method = selectMethod (form.size () - 1);
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

    public Object apply (final Object... arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Method method = selectMethod (arguments.length);
	if (method.isVarArgs ())
	{
	    final Object[] vargs =
		{arguments};
	    final Object result = method.invoke (obj, vargs);
	    return result;
	}
	else
	{
	    final Object result = method.invoke (obj, arguments);
	    return result;
	}
    }

    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target
     * @return
     */
    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Object", obj);
	for (final Method method : methods)
	{
	    result.put ("Method", method);
	}
	super.getDescriberValues (target);
	return result;
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
