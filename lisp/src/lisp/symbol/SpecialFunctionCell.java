
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;

import lisp.Symbol;
import lisp.eval.LexicalContext;

public class SpecialFunctionCell extends FunctionCell
{
    private ObjectMethod[] methods;

    public SpecialFunctionCell (final Symbol symbol)
    {
	super (symbol, false);
	methods = new ObjectMethod[] {};
	makeOverloadMap (methods);
    }

    public SpecialFunctionCell (final Symbol symbol, final Object obj, final Method method, final String documentation)
    {
	super (symbol, false);
	methods = new ObjectMethod[]
	    {new ObjectMethod (obj, method, documentation)};
	makeOverloadMap (methods);
    }

    @Override
    public void overload (final Object obj, final Method method, final String documentation)
    {
	final ObjectMethod[] newMethods = Arrays.copyOf (methods, methods.length + 1, ObjectMethod[].class);
	newMethods[methods.length] = new ObjectMethod (obj, method, documentation);
	// Scan methods and determine if there are possible ambiguous ones
	makeOverloadMap (newMethods);
	methods = newMethods;
    }

    @Override
    public Object eval (final LexicalContext context, final List<?> form) throws Exception
    {
	// Form size is one extra due to the function name &
	// Number of arguments is one extra due to the interpreter argument.
	final ObjectMethod method = selectMethod (form.size ());
	if (method.isVarArgs ())
	{
	    return applyVarArgs (context, method, form);
	}
	else
	{
	    return applyFixedArgs (context, method, form);
	}
    }

    private Object applyVarArgs (final LexicalContext context, final ObjectMethod method, final List<?> form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	// Number of parameters excluding the interpreter
	final int paramsLengthActual = parameters.length - 1;
	final Object[] arguments = new Object[parameters.length];
	arguments[0] = context;
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
	return method.method.invoke (method.object, arguments);
    }

    private Object applyFixedArgs (final LexicalContext context, final ObjectMethod method, final List<?> form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	// Form includes an extra element for the function name
	// parameters includes an extra element for the interpreter
	final Object[] arguments = new Object[parameters.length];
	arguments[0] = context;
	for (int i = 1; i < parameters.length; i++)
	{
	    arguments[i] = form.get (i);
	}
	return method.method.invoke (method.object, arguments);
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    @Override
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	super.getDescriberValues (result, target);
	for (final ObjectMethod method : methods)
	{
	    result.put ("Method", method);
	}
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
