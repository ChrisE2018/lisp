
package lisp.cc;

import java.lang.reflect.*;

import lisp.*;
import lisp.eval.*;

public class CompilerPrimitives extends Definer
{
    // (define foo (x) alpha)
    // (define foo (a b) (+ 3 4))
    @DefineLisp (special = true, name = "define")
    public Symbol define (final Interpreter interpreter, final Symbol functionName, final LispList args, final Object... forms)
    {
	try
	{
	    final LispList body = new LispList ();
	    for (final Object f : forms)
	    {
		body.add (f);
	    }
	    System.out.printf ("Compiling %s as %s %n", functionName, body);
	    final Class<?> c = createCompiledFunction (functionName, args, body);
	    System.out.printf ("Compiled %s %n", functionName);
	    System.out.printf ("List of Declared Methods%n");
	    for (final Method method : c.getDeclaredMethods ())
	    {
		System.out.printf ("* Method: %s %n", method);
	    }
	    System.out.printf ("%n");
	}
	catch (final Exception e)
	{
	    e.printStackTrace ();
	}
	return functionName;
    }

    /** Create a compiled function definition from lisp sources. */
    private Class<?> createCompiledFunction (final Symbol symbol, final LispList args, final LispList body)
            throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException
    {
	// Need to accept function arguments.
	// [TODO] Need to handle nested forms
	// [TODO] Constants like symbols need to be created in the compiled class <init> method.
	final CompileLoader cl = new CompileLoader ();

	final String methodName = symbol.gensym ().getName ();
	final Class<?> cls = cl.compile (methodName, args, body);
	final Class<?>[] parameterTypes = new Class<?>[args.size ()];
	for (int i = 0; i < parameterTypes.length; i++)
	{
	    parameterTypes[i] = java.lang.Object.class;
	}
	final Method method = cls.getDeclaredMethod (methodName, parameterTypes);
	FunctionCell function = symbol.getFunction ();
	// Overloading requires adding the method to an existing function cell
	// [TODO] Lexical bindings
	if (function != null)
	{
	    function.overload (null, method);
	}
	else
	{
	    final Class<?>[] types =
		{int.class};
	    final Constructor<?> con = cls.getConstructor (types);
	    System.out.printf ("Calling newInstance(1)%n");
	    final Object instance = con.newInstance (1);
	    function = new StandardFunctionCell (instance, method);
	    symbol.setFunction (function);
	}
	return cls;
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
