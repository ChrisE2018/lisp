
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;

import org.objectweb.asm.tree.ClassNode;

import lisp.Symbol;
import lisp.eval.LexicalContext;

public class StandardFunctionCell extends FunctionCell
{
    private ObjectMethod[] methods;

    public StandardFunctionCell (final Symbol symbol)
    {
	super (symbol, false);
	methods = new ObjectMethod[] {};
	makeOverloadMap (methods);
    }

    public StandardFunctionCell (final Symbol symbol, final Object obj, final Method method, final String documentation)
    {
	super (symbol, false);
	methods = new ObjectMethod[] {new ObjectMethod (obj, method, documentation)};
	makeOverloadMap (methods);
    }

    public StandardFunctionCell (final Symbol symbol, final Object obj, final Method method, final String documentation,
            final Object source, final ClassNode cn)
    {
	super (symbol, false);
	methods = new ObjectMethod[] {new ObjectMethod (obj, method, documentation, source, cn)};
	makeOverloadMap (methods);
    }

    // @Override
    // public void overload (final Object obj, final Method method, final String documentation,
    // final LispList source,
    // final MethodNode mn)
    // {
    // overload (obj, method, documentation, source, mn);
    // }

    @Override
    public void overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
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
		    methods[i] = new ObjectMethod (obj, method, documentation);
		}
	    }
	    makeOverloadMap (methods);
	    return;
	}
	final ObjectMethod[] newMethods = Arrays.copyOf (methods, methods.length + 1, ObjectMethod[].class);
	newMethods[methods.length] = new ObjectMethod (obj, method, documentation, source, cn);
	// Scan methods and determine if there are possible ambiguous ones
	makeOverloadMap (newMethods);
	methods = newMethods;
    }

    @Override
    public Object eval (final LexicalContext context, final List<?> form) throws Exception
    {
	final ObjectMethod method = selectMethod (form.size () - 1);
	if (method.isVarArgs ())
	{
	    return applyVarArgs (context, method, form);
	}
	else
	{
	    return applyFixedArgs (context, method, form);
	}
    }

    private Object applyVarArgs (final LexicalContext context, final ObjectMethod method, final List<?> form) throws Exception
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] arguments = new Object[parameters.length];
	// Collect the required arguments
	for (int i = 1; i < parameters.length; i++)
	{
	    final Object f = form.get (i);
	    arguments[i - 1] = context.eval (f);
	}
	// Collect the optional arguments
	final int count = form.size () - parameters.length;
	final Object[] args = new Object[count];
	for (int i = 0; i < count; i++)
	{

	    final Object f = form.get (parameters.length + i);
	    args[i] = context.eval (f);
	}
	arguments[parameters.length - 1] = args;
	return method.method.invoke (method.object, arguments);
    }

    private Object applyFixedArgs (final LexicalContext context, final ObjectMethod method, final List<?> form) throws Exception
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] arguments = new Object[parameters.length];
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments[i - 1] = context.eval (f);
	}
	return method.method.invoke (method.object, arguments);
    }

    @Override
    public Object apply (final Object... arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final ObjectMethod method = selectMethod (arguments.length);
	if (method.isVarArgs ())
	{
	    final Method m = method.method;
	    final int argCount = m.getParameterTypes ().length;
	    final Object[] args = new Object[argCount];
	    for (int i = 0; i < argCount - 1; i++)
	    {
		args[i] = arguments[i];
	    }
	    final Object[] vargs = new Object[arguments.length + 1 - argCount];
	    for (int i = 0; i < vargs.length; i++)
	    {
		vargs[i] = arguments[i + argCount - 1];
	    }
	    args[argCount - 1] = vargs;
	    return method.method.invoke (method.object, args);
	}
	else
	{
	    return method.method.invoke (method.object, arguments);
	}
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
	for (int i = 0; i < methods.length; i++)
	{
	    final ObjectMethod m = methods[i];
	    result.put ("Method", m);
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
