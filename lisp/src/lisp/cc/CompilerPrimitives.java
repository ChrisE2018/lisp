
package lisp.cc;

import java.io.IOException;
import java.lang.reflect.*;
import java.util.logging.Logger;

import lisp.*;
import lisp.eval.*;
import lisp.symbol.*;

public class CompilerPrimitives extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (CompilerPrimitives.class.getName ());

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
	    // [TODO] If functionName looks like (the <type> <name>) then declare a return type.
	    LOGGER.info (String.format ("Compiling %s as %s", functionName, body));
	    final Class<?> c = createCompiledFunction (functionName, args, body);
	    LOGGER.info (String.format ("Compiled %s", functionName));
	    LOGGER.info (String.format ("List of Declared Methods"));
	    for (final Method method : c.getDeclaredMethods ())
	    {
		LOGGER.info (String.format ("* Method: %s", method));
	    }
	    LOGGER.info ("");
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
	final CompileLoader cl = new CompileLoader ();

	final String methodName = symbol.gensym ().getName ();
	final String documentation = (body.get (0) instanceof String) ? (String)(body.get (0)) : "";

	final Class<?> cls = cl.compile (methodName, args, body);
	final Class<?>[] parameterTypes = new Class<?>[args.size ()];
	for (int i = 0; i < parameterTypes.length; i++)
	{
	    parameterTypes[i] = java.lang.Object.class;
	}

	final Class<?>[] types =
	    {int.class};
	final Constructor<?> con = cls.getConstructor (types);
	// System.out.printf ("Calling newInstance(1)%n");
	final Object instance = con.newInstance (1);

	final Method method = cls.getDeclaredMethod (methodName, parameterTypes);
	FunctionCell function = symbol.getFunction ();
	// Overloading requires adding the method to an existing function cell
	if (function != null && !(function instanceof DefFunctionCell))
	{
	    function.overload (instance, method, documentation);
	}
	else
	{
	    function = new StandardFunctionCell (symbol, instance, method, documentation);
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

    @DefineLisp (special = true)
    public Object timing (final LexicalContext context, final Object expr, final Object n) throws Exception
    {
	// (define f (n) (if (<= n 1) 1 (* n (f (1- n)))))
	// (def g (n) (if (<= n 1) 1 (* n (g (1- n)))))
	// (timing (f 10) (* 1000 1000))
	// ==> Completed 1000000 iterations in 2.185586 seconds = 0.000002 seconds / iteration
	// (timing (g 10) (* 1000 1000))
	// ==> Completed 1000000 iterations in 4.815515 seconds = 0.000005 seconds / iteration
	// (define ff () (repeat 1000 (f 10)))
	// (def gg () (repeat 1000 (g 10)))
	// (timing (ff) 1000)
	// ==> Completed 1000 iterations in 1.816591 seconds = 0.001817 seconds / iteration
	// (timing (gg) 1000)
	// ==> Completed 1000 iterations in 4.962463 seconds = 0.004962 seconds / iteration
	final int count = (Integer)context.eval (n);
	final long start = System.nanoTime ();
	for (int i = 0; i < count; i++)
	{
	    context.eval (expr);
	}
	final long duration = System.nanoTime () - start;
	final double seconds = duration / (1000.0 * 1000.0 * 1000.0);
	System.out.printf ("Completed %s iterations in %f seconds = %f seconds / iteration %n", count, seconds, seconds / count);
	return null;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
