
package lisp.demo;

import java.io.IOException;
import java.util.logging.*;

import lisp.cc.VerifyPrimitives;
import lisp.eval.*;
import lisp.gui.*;
import lisp.lang.*;
import lisp.lang.Package;

/** Simple toplevel loop that reads a lisp form, evaluates it and prints the result. */
public class Repl
{
    private static final LogManager logManager = LogManager.getLogManager ();
    private static final Logger LOGGER = Logger.getLogger (Repl.class.getName ());

    private static boolean running = true;

    private final Interpreter interpreter;

    public static void main (final String[] args)
    {
	try
	{
	    final Repl repl = new Repl (args);
	    final LispStream stream = new LispInputStream (System.in);
	    repl.toplevel (stream);
	}
	catch (final java.lang.reflect.InvocationTargetException e)
	{
	    Throwable ee = e;
	    for (int i = 0; i < 10 && ee instanceof java.lang.reflect.InvocationTargetException; i++)
	    {
		ee = ee.getCause ();
	    }
	    VerifyPrimitives.incrementReplErrorCount ("Initialization evaluation error " + ee);
	    LOGGER.log (Level.SEVERE, "Initialization evaluation error", ee);
	}
	catch (final Throwable e)
	{
	    VerifyPrimitives.incrementReplErrorCount ("Miscellaneous initialization error " + e);
	    LOGGER.log (Level.SEVERE, "Miscellaneous initialization error", e);
	}
    }

    public static void exit ()
    {
	running = false;
    }

    /**
     * Constructor for demo application.
     *
     * @throws Exception
     */
    private Repl (final String[] args) throws Exception
    {
	logManager.readConfiguration (Interactor.class.getResource ("loggingBootstrap.properties").openStream ());
	interpreter = new Interpreter ();
	final Application application = new Application (interpreter);
	application.initialize (args);
    }

    public Repl (final Interpreter interpreter) throws SecurityException, IOException
    {
	logManager.readConfiguration (Interactor.class.getResource ("loggingBootstrap.properties").openStream ());
	this.interpreter = interpreter;
    }

    public void toplevel (final LispStream stream)
    {
	LOGGER.info ("Starting REPL toplevel");
	int index = 0;
	while (running)
	{
	    try
	    {
		Thread.sleep (100);
		rep (stream, ++index);
	    }
	    catch (final java.lang.reflect.InvocationTargetException e)
	    {
		Throwable ee = e;
		for (int i = 0; i < 10 && ee instanceof java.lang.reflect.InvocationTargetException; i++)
		{
		    ee = ee.getCause ();
		}
		VerifyPrimitives.incrementReplErrorCount ("REPL evaluation error " + ee);
		LOGGER.log (Level.SEVERE, "REPL evaluation error", ee);
	    }
	    catch (final Throwable e)
	    {
		VerifyPrimitives.incrementReplErrorCount ("Miscellaneous REPL error " + e);
		LOGGER.log (Level.SEVERE, "Miscellaneous REPL error", e);
	    }
	}
    }

    private void rep (final LispStream stream, final int index) throws Exception
    {
	final Package pkg = PackageFactory.getCurrentPackage ();
	final Symbol e = pkg.internSymbol ("e" + index);
	System.out.printf ("[%s] ", e);
	Object form = null;
	try
	{
	    final LispReader lispReader = LispReader.getLispThreadReader ();
	    form = lispReader.read (stream, pkg);
	}
	catch (final Throwable ex)
	{
	    try
	    {
		System.out.printf ("Error reading expression: %s\n", ex);
		VerifyPrimitives.incrementReplErrorCount ("Error reading expression: " + ex);
		// Read to a newline character
		while (stream.read () != '\n')
		{

		}
	    }
	    catch (final Throwable exx)
	    {
		VerifyPrimitives.incrementReplErrorCount ("Error recovering from error: " + exx);
		System.out.printf ("[Error recovering from error: %s]\n", exx);
	    }
	    return;
	}
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	e.setValue (form);
	final StringBuilder buffer = new StringBuilder ();
	// buffer.append (form.toString ());
	// System.out.println (buffer);
	final Symbol v = pkg.internSymbol ("v" + index);
	final Symbol t = pkg.internSymbol ("t" + index);
	final Symbol repeat = pkg.internSymbol ("*repeat*");
	final int repeatCount = repeat.getIntValue (1);
	System.out.printf ("[%s] ==> ", v);
	final LexicalContext context = new LexicalContext (interpreter);
	final long startTime = System.currentTimeMillis ();
	final Object value = context.eval (form);
	for (int i = 1; i < repeatCount; i++)
	{
	    context.eval (form);
	}
	final long duration = System.currentTimeMillis () - startTime;
	// Provide time for output to display
	Thread.sleep (50);
	v.setValue (value);
	t.setValue (duration);
	buffer.setLength (0);
	if (value == null)
	{
	    buffer.append ("null");
	}
	else
	{
	    LispReader.printElement (buffer, value);
	}
	buffer.append ("\n");
	buffer.append (duration);
	buffer.append (" ms");
	if (repeatCount > 1)
	{
	    // buffer.append (" in ");
	    // buffer.append (repeatCount);
	    // buffer.append ("iterations. ");
	    final double tt = (double)duration / repeatCount;
	    buffer.append (String.format (" %.3f ms/iteration. ", tt));
	}
	buffer.append ("\n");
	System.out.print (buffer);
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
