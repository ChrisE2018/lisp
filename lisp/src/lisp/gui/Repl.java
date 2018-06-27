
package lisp.gui;

import java.io.IOException;
import java.util.logging.*;

import lisp.cc.VerifyPrimitives;
import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;

/** Simple toplevel loop that reads a lisp form, evaluates it and prints the result. */
public class Repl
{
    private static final LogManager logManager = LogManager.getLogManager ();
    private static final Logger LOGGER = Logger.getLogger (Repl.class.getName ());

    private static boolean running = true;

    private final Interpreter interpreter;
    private Symbol star;
    private Symbol star2;
    private Symbol star3;
    private Symbol plus;
    private Symbol plus2;
    private Symbol plus3;

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
	Object form = null;
	final LispReader lispReader = LispReader.getLispThreadReader ();
	final Package pkg = lispReader.getCurrentPackage ();
	final Symbol e = lispReader.readSymbol (pkg, "e" + index);
	try
	{
	    System.out.printf ("[%s] ", e);
	    star = lispReader.readSymbol (pkg, "*");
	    star2 = lispReader.readSymbol (pkg, "**");
	    star3 = lispReader.readSymbol (pkg, "***");
	    plus = lispReader.readSymbol (pkg, "+");
	    plus2 = lispReader.readSymbol (pkg, "++");
	    plus3 = lispReader.readSymbol (pkg, "+++");
	    form = lispReader.read (stream, pkg);
	    plus3.setValue (plus2.getValue (null));
	    plus2.setValue (plus.getValue (null));
	    plus.setValue (form);
	    e.setValue (form);
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
	// if (form == null)
	// {
	// System.out.println ("Exit");
	// return;
	// }
	final StringBuilder buffer = new StringBuilder ();
	final Symbol v = lispReader.readSymbol (pkg, "v" + index);
	final Symbol t = lispReader.readSymbol (pkg, "t" + index);
	System.out.printf ("[%s] ==> ", v);
	final LexicalContext context = new LexicalContext (interpreter);
	final long startTime = System.currentTimeMillis ();
	final Object value = context.eval (form);
	final long duration = System.currentTimeMillis () - startTime;
	star3.setValue (star2.getValue (null));
	star2.setValue (star.getValue (null));
	star.setValue (value);
	v.setValue (value);
	t.setValue (duration);
	// Provide time for output to display
	Thread.sleep (50);
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
	// TODO Make display of duration configurable.
	buffer.append (duration);
	buffer.append (" ms");
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
