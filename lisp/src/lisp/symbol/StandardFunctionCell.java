
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;

import lisp.Symbol;
import lisp.eval.*;

public class StandardFunctionCell extends FunctionCell
{
    private ObjectMethod[] methods;

    public StandardFunctionCell (final Symbol symbol, final Object obj, final Method method)
    {
	super (symbol);
	methods = new ObjectMethod[]
	    {new ObjectMethod (obj, method)};
	makeOverloadMap (methods);
    }

    @Override
    public void overload (final DefineLisp a, final Object obj, final Method method)
    {
	final int c = getMethodSelectorCount (method);
	final ObjectMethod previousDefinition = getOverload (c);
	if (previousDefinition != null)
	{
	    System.out.printf ("Redefining %s as %s %n", previousDefinition, method);
	    for (int i = 0; i < methods.length; i++)
	    {
		final ObjectMethod m = methods[i];
		if (m.method == previousDefinition.method)
		{
		    methods[i] = new ObjectMethod (obj, method);
		}
	    }
	    makeOverloadMap (methods);
	    return;
	}
	final ObjectMethod[] newMethods = Arrays.copyOf (methods, methods.length + 1, ObjectMethod[].class);
	newMethods[methods.length] = new ObjectMethod (obj, method);
	// Scan methods and determine if there are possible ambiguous ones
	makeOverloadMap (newMethods);
	methods = newMethods;
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final ObjectMethod method = selectMethod (form.size () - 1);
	if (method.isVarArgs ())
	{
	    return applyVarArgs (interpreter, method, form);
	}
	else
	{
	    return applyFixedArgs (interpreter, method, form);
	}
    }

    private Object applyVarArgs (final Interpreter interpreter, final ObjectMethod method, final List<?> form) throws Exception
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
	return method.method.invoke (method.object, arguments);
    }

    private Object applyFixedArgs (final Interpreter interpreter, final ObjectMethod method, final List<?> form) throws Exception
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] arguments = new Object[parameters.length];
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments[i - 1] = interpreter.eval (f);
	}
	return method.method.invoke (method.object, arguments);
    }

    public Object apply (final Object... arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final ObjectMethod method = selectMethod (arguments.length);
	if (method.isVarArgs ())
	{
	    final Object[] vargs =
		{arguments};
	    final Object result = method.method.invoke (method.object, vargs);
	    return result;
	}
	else
	{
	    final Object result = method.method.invoke (method.object, arguments);
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
	for (int i = 0; i < methods.length; i++)
	{
	    final ObjectMethod m = methods[i];
	    result.put ("Object", m.object);
	    result.put ("Method", m.method);
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
	buffer.append (" ");
	buffer.append (getFunctionName ());
	for (final ObjectMethod m : methods)
	{
	    buffer.append (" ");
	    buffer.append (m);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
