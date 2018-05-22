
package lisp.cc;

import java.io.IOException;
import java.lang.reflect.*;

import lisp.*;
import lisp.eval.*;
import lisp.symbol.*;

public class CompilerPrimitives extends Definer
{
    // (define foo (x) alpha)
    // (define foo (a b) (+ 3 4))
    @DefineLisp (special = true, name = "define")
    public Symbol define (@SuppressWarnings ("unused") final LexicalContext context, final Symbol functionName,
            final LispList args, final Object... forms)
    {
	try
	{
	    // [TODO] If first form is a string it should be saved as documentation. The
	    // FunctionCell should have a field to store documentation.
	    // [TODO] Before the functionName we should require a return type. The package system
	    // should mirror the java package system, so java.lang.Object would be an allowed type,
	    // with abbreviation of Object allowed. The Symbol class should have a getClass method
	    // that returns the corresponding Java type. Declarations associated with the function
	    // definition should determine if a compiled call will strongly depend on the current
	    // definition.
	    // [TODO] Function calls should not create new argument arrays. Use exception handling
	    // to detect the case where a function definition changes since it was compiled.
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

    /**
     * Create a compiled function definition from lisp sources.
     *
     * @throws IOException
     */
    private Class<?> createCompiledFunction (final Symbol symbol, final LispList args, final LispList body)
            throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException, IOException
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

	final Class<?>[] types =
	    {int.class};
	final Constructor<?> con = cls.getConstructor (types);
	System.out.printf ("Calling newInstance(1)%n");
	final Object instance = con.newInstance (1);

	final Method method = cls.getDeclaredMethod (methodName, parameterTypes);
	FunctionCell function = symbol.getFunction ();
	// Overloading requires adding the method to an existing function cell
	if (function != null && !(function instanceof DefFunctionCell))
	{
	    function.overload (instance, method);
	}
	else
	{
	    function = new StandardFunctionCell (symbol, instance, method);
	    symbol.setFunction (function);
	}
	return cls;
    }

    private int testCount = 0;
    private int passCount = 0;
    private int failCount = 0;
    private int errorCount = 0;

    @DefineLisp
    public Object resetTestStatistics ()
    {
	testCount = 0;
	passCount = 0;
	failCount = 0;
	errorCount = 0;
	return null;
    }

    @DefineLisp
    public Object printTestStatistics ()
    {
	if (testCount > 0)
	{
	    System.out.printf ("%n");
	    System.out.printf ("%6s %4d of %4d %4.1f%%%n", "Pass", passCount, testCount, (passCount * 100.0 / testCount));
	    if (failCount > 0)
	    {
		System.out.printf ("%6s %4d of %4d %4.1f%%%n", "Fail", failCount, testCount, (failCount * 100.0 / testCount));
	    }
	    if (errorCount > 0)
	    {
		System.out.printf ("%6s %4d of %4d %4.1f%%%n", "Error", errorCount, testCount, (errorCount * 100.0 / testCount));
	    }
	    System.out.printf ("%6s %4d of %4d %4.1f%%%n", "Total", testCount, testCount, (testCount * 100.0 / testCount));
	    if (passCount == testCount && failCount == 0 && errorCount == 0)
	    {
		System.out.printf ("%nAll tests passed!%n");
	    }
	    else
	    {
		System.out.printf ("%nThere were test failures%n");
	    }
	}
	else
	{
	    System.out.printf ("No tests have been run. Call verify to submit test data.%n");
	}
	return null;
    }

    @DefineLisp (special = true)
    public Object verify (final LexicalContext context, final Object expr, final Object expect)
    {
	try
	{
	    testCount++;
	    final Object value = context.eval (expr);
	    final Object expected = context.eval (expect);
	    if (same (value, expected))
	    {
		System.err.printf ("Pass: value of %s is %s while expecting %s%n", expr, value, expected);
		passCount++;
	    }
	    else
	    {
		System.err.printf ("Fail: value of %s is %s while expecting %s%n", expr, value, expected);
		failCount++;
	    }
	}
	catch (final Throwable e)
	{
	    System.err.printf ("Error: while evaluating %s: %s%n", expr, e);
	    errorCount++;
	}
	return null;
    }

    @DefineLisp (special = true)
    public Object verifyError (final LexicalContext context, final Object expr, final String expected)
    {
	try
	{
	    testCount++;
	    final Object value = context.eval (expr);
	    if (same (value, expected))
	    {
		System.err.printf ("Fail: value of %s is %s while expecting error %s%n", expr, value, expected);
		failCount++;
	    }
	    else
	    {
		System.err.printf ("Fail: value of %s is %s while expecting error %s%n", expr, value, expected);
		failCount++;
	    }
	}
	catch (final Throwable e)
	{
	    if (e.getMessage ().equals (expected))
	    {
		System.err.printf ("Pass: Expected error %s found while evaluating %s%n", expected, expr);
		passCount++;
	    }
	    else
	    {
		System.err.printf ("Fail: Expected error %s not found while evaluating %s: %s%n", expected, expr, e);
		passCount++;
	    }
	}
	return null;
    }

    private boolean same (final Object a, final Object b)
    {
	if (a == b)
	{
	    return true;
	}
	if (a == null && b != null)
	{
	    return false;
	}
	if (a != null && b == null)
	{
	    return false;
	}
	return a.equals (b);
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
