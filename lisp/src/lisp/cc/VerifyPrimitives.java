
package lisp.cc;

import java.util.*;

import lisp.eval.*;
import lisp.lang.*;
import lisp.special.The;

public class VerifyPrimitives extends Definer
{
    private static The the = new The ();

    // private static int replErrorCount = 0;

    private static final List<Symbol> verifyFailures = new ArrayList<Symbol> ();
    private static final List<String> verifyErrors = new ArrayList<String> ();

    /**
     * Record an error. Originally this just stored the count. It was modified to record a message
     * but several function names are still based on count.
     *
     * @param message
     */
    public static void incrementReplErrorCount (final String message)
    {
	verifyErrors.add (message);
    }

    public static int getReplErrorCount ()
    {
	return verifyErrors.size ();
    }

    public static void resetReplErrorCount ()
    {
	verifyErrors.clear ();
    }

    @DefineLisp
    public int getErrorCount ()
    {
	return verifyErrors.size ();
    }

    @DefineLisp
    public int resetErrorCount ()
    {
	final int value = verifyErrors.size ();
	verifyErrors.clear ();
	return value;
    }

    public static List<String> getVerifyErrors ()
    {
	return verifyErrors;
    }

    public static List<Symbol> getVerifyFailures ()
    {
	return verifyFailures;
    }

    private boolean stopAtFirstFailure = false;

    private Symbol verifyPhase = null;

    private long startTime = System.currentTimeMillis ();

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
	verifyFailures.clear ();
	startTime = System.currentTimeMillis ();
	return null;
    }

    @DefineLisp
    public Object printTestStatistics ()
    {
	if (testCount > 0)
	{
	    final long durationMs = System.currentTimeMillis () - startTime;
	    final double durationSecs = durationMs / 1000.0;
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
	    if (verifyErrors.size () > 0)
	    {
		System.out.printf ("%nThere were %d exceptions during testing%n", verifyErrors.size ());
		for (int i = 0; i < verifyErrors.size (); i++)
		{
		    System.out.printf ("[%d] %s%n", i + 1, verifyErrors.get (i));
		}
	    }
	    else if (passCount == testCount && failCount == 0 && errorCount == 0)
	    {
		System.out.printf ("%nAll tests passed in %.2f seconds!%n", durationSecs);
	    }
	    else
	    {
		System.out.printf ("%nThere were test failures in %.2f seconds%n", durationSecs);
	    }
	    for (final Symbol s : verifyFailures)
	    {
		System.out.printf ("Failure: %s%n", s);
	    }
	}
	else
	{
	    System.out.printf ("No tests have been run. Call verify to submit test data.%n");
	}
	if (verifyErrors.size () > 0)
	{
	    System.out.printf ("%nThere were %d exceptions during testing%n", verifyErrors.size ());
	    for (int i = 0; i < verifyErrors.size (); i++)
	    {
		System.out.printf ("[%d] %s%n", i + 1, verifyErrors.get (i));
	    }
	}
	return null;
    }

    /**
     * Save a marker to include in verify messages. This is intended to mark sections of tests to
     * help find a problem.
     *
     * @param context
     */
    @DefineLisp (special = true)
    public Object verifyPhase (final LexicalContext context, final Symbol phase)
    {
	System.out.printf ("%nCompleted verification phase %s%n", verifyPhase);
	printTestStatistics ();

	verifyPhase = phase;
	System.out.printf ("%nStarting verification phase %s%n%n", phase);
	return verifyPhase;
    }

    private Symbol getVerifyPhase ()
    {
	if (verifyPhase == null)
	{
	    verifyPhase = Symbol.named ("p");
	}
	return verifyPhase.gensym ();
    }

    @DefineLisp
    public void stopAtFirstFailure (@SuppressWarnings ("hiding") final boolean stopAtFirstFailure)
    {
	this.stopAtFirstFailure = stopAtFirstFailure;
    }

    private void checkStopAtFirstFailure ()
    {
	if (stopAtFirstFailure)
	{
	    printTestStatistics ();
	    System.exit (1);
	}
    }

    @DefineLisp (special = true)
    public Object verification (final LexicalContext context, final Symbol phase, final Object setup, final Object... checks)
    {
	final Symbol oldVerifyPhase = verifyPhase;
	try
	{
	    if (verifyPhase != null)
	    {
		System.out.printf ("%nCompleted verification phase %s%n", verifyPhase);
		printTestStatistics ();
	    }

	    verifyPhase = phase;
	    System.out.printf ("%nStarting verification phase %s%n%n", phase);
	    try
	    {
		context.eval (setup);
	    }
	    catch (final Throwable e)
	    {

		verifyFailures.add (phase);
		System.out.printf ("[%s] Error: during setup: %s%n", phase, e);
		errorCount++;
		checkStopAtFirstFailure ();
		return null;
	    }
	    verificationInternals (context, phase, checks);
	}
	catch (final Throwable e)
	{
	    verifyFailures.add (phase);
	    System.out.printf ("[%s] Error: unexpected internal error: %s%n", phase, e);
	    errorCount++;
	    checkStopAtFirstFailure ();
	}
	finally
	{
	    verifyPhase = oldVerifyPhase;
	}
	return null;
    }

    private void verificationInternals (final LexicalContext context, final Symbol phase, final Object[] checks)
    {
	for (int i = 0; i < checks.length; i++)
	{
	    final LispList check = (LispList)checks[i];
	    try
	    {
		// check1 (context, phase, check);
		context.eval (check);
	    }
	    catch (final Throwable e)
	    {
		verifyFailures.add (phase);
		System.out.printf ("[%s] Error: while evaluating %s: %s%n", phase, check, e);
		errorCount++;
		checkStopAtFirstFailure ();
		return;
	    }
	}
    }

    @DefineLisp (special = true)
    public boolean verify (final LexicalContext context, final Object expr)
    {
	return verify (context, expr, true);
    }

    @DefineLisp (special = true)
    public boolean verify (final LexicalContext context, final Object expr, final Object expect)
    {
	final Symbol phase = getVerifyPhase ();
	testCount++;
	try
	{

	    final Object value = context.eval (expr);
	    final Object expected = context.eval (expect);
	    if (same (value, expected))
	    {
		System.out.printf ("[%s] Pass: value of %s is %s while expecting %s%n", phase, expr, value, expected);
		passCount++;
		return true;
	    }
	    else
	    {
		verifyFailures.add (phase);
		System.out.printf ("[%s] Fail: value of %s is %s while expecting %s%n", phase, expr, value, expected);
		failCount++;
		checkStopAtFirstFailure ();
		return false;
	    }
	}
	catch (final java.lang.reflect.InvocationTargetException originalEx)
	{
	    Throwable e = originalEx;
	    for (int i = 0; i < 10 && e instanceof java.lang.reflect.InvocationTargetException; i++)
	    {
		e = e.getCause ();
	    }
	    verifyFailures.add (phase);
	    System.out.printf ("[%s] Error: while evaluating %s: %s%n", phase, expr, e);
	    errorCount++;
	    checkStopAtFirstFailure ();
	    return false;
	}
	catch (final Throwable e)
	{
	    verifyFailures.add (phase);
	    System.out.printf ("[%s] Error: while evaluating %s: %s%n", phase, expr, e);
	    errorCount++;
	    checkStopAtFirstFailure ();
	    return false;
	}
    }

    @DefineLisp (special = true)
    public Object verifyError (final LexicalContext context, final Object expr, final String expected)
    {
	final Symbol phase = getVerifyPhase ();
	testCount++;
	try
	{
	    final Object value = context.eval (expr);
	    verifyFailures.add (phase);
	    System.out.printf ("[%s] Fail: value of %s is %s while expecting error %s%n", phase, expr, value, expected);
	    failCount++;
	    checkStopAtFirstFailure ();
	}
	catch (final Throwable e)
	{
	    if (e.getClass ().getName ().equals (expected))
	    {
		System.out.printf ("[%s] Pass: Expected error %s found while evaluating %s%n", phase, expected, expr);
		passCount++;
	    }
	    else
	    {
		verifyFailures.add (phase);
		System.out.printf ("[%s] Fail: Expected error %s not found while evaluating %s: %s%n", phase, expected, expr, e);
		failCount++;
		checkStopAtFirstFailure ();
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
	return the.isequalp (a, b);
    }

    @DefineLisp (special = true)
    public Object timing (final LexicalContext context, final Object n, final Object expr) throws Exception
    {
	// (define f (n) (if (<= n 1) 1 (* n (f (1- n)))))
	// (def g (n) (if (<= n 1) 1 (* n (g (1- n)))))
	// (timing (* 1000 1000) (f 10))
	// ==> Completed 1000000 iterations in 2.185586 seconds = 0.000002 seconds / iteration
	// (timing (* 1000 1000) (g 10))
	// ==> Completed 1000000 iterations in 4.815515 seconds = 0.000005 seconds / iteration
	// (define ff () (repeat 1000 (f 10)))
	// (def gg () (repeat 1000 (g 10)))
	// (timing 1000 (ff))
	// ==> Completed 1000 iterations in 1.816591 seconds = 0.001817 seconds / iteration
	// (timing 1000 (gg))
	// ==> Completed 1000 iterations in 4.962463 seconds = 0.004962 seconds / iteration
	final int count = (Integer)context.eval (n);
	final long start = System.nanoTime ();
	for (int i = 0; i < count; i++)
	{
	    context.eval (expr);
	}
	final long duration = System.nanoTime () - start;
	final double seconds = duration / (1000.0 * 1000.0 * 1000.0);
	final double average = seconds / count;
	System.out.printf ("Completed %s iterations in %f seconds = %f seconds / iteration %n", count, seconds, average);
	System.out.printf ("   Each iteration evaluated: %s %n", expr);
	return average;
    }
}
