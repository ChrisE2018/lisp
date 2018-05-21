
package lisp.demo;

import lisp.*;
import lisp.Package;
import lisp.eval.Interpreter;

/** Simple toplevel loop that reads a lisp form, evaluates it and prints the result. */
public class Repl
{
    private final LispReader reader;

    private final Interpreter interpreter;

    public static void main (final String[] args)
    {
	try
	{
	    final Repl repl = new Repl (args);
	    final LispStream stream = new LispStream (System.in);
	    repl.toplevel (stream);
	}
	catch (final java.lang.reflect.InvocationTargetException e)
	{
	    Throwable ee = e;
	    for (int i = 0; i < 10 && ee instanceof java.lang.reflect.InvocationTargetException; i++)
	    {
		ee = ee.getCause ();
	    }
	    System.out.printf ("Initialization error %s %n", ee);
	    ee.printStackTrace ();
	}
	catch (final Throwable e)
	{
	    System.out.printf ("Initialization error %s %n", e);
	    e.printStackTrace ();
	}
    }

    /**
     * Constructor for demo application.
     *
     * @throws Exception
     */
    private Repl (final String[] args) throws Exception
    {
	// [TODO] Move argument processing into Interpreter class
	interpreter = new Interpreter ();
	reader = new LispReader ();
	for (int i = 1; i < args.length; i++)
	{
	    final String key = args[i - 1];
	    final String value = args[i];
	    // [TODO] --setq "var=form"
	    // [TODO] --package pkg
	    // [TODO] --log log4jconfiguration
	    if (key.equals ("-l") || key.equals ("--load"))
	    {
		if (!interpreter.loadResource (value))
		{
		    interpreter.loadFile (value);
		}
	    }
	}
    }

    /** Constructor to use an interpreter built elsewhere. */
    public Repl (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
	reader = new LispReader ();
    }

    public void toplevel (final LispStream stream)
    {
	int index = 0;
	while (true)
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
		ee.printStackTrace ();
	    }
	    catch (final Throwable e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    private void rep (final LispStream stream, final int index) throws Exception
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	final Symbol e = pkg.internPrivate ("e" + index);
	System.out.printf ("[%s] ", e);
	Object form = null;
	try
	{
	    form = reader.read (stream, pkg);
	}
	catch (final Throwable ex)
	{
	    try
	    {
		System.out.printf ("Error reading expression: %s\n", ex);
		// Read to a newline character
		while (stream.read () != '\n')
		{

		}
	    }
	    catch (final Throwable exx)
	    {
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
	final Symbol v = pkg.internPrivate ("v" + index);
	final Symbol t = pkg.internPrivate ("t" + index);
	final Symbol repeat = pkg.internPrivate ("*repeat*");
	final int repeatCount = repeat.getIntValue (1);
	System.out.printf ("[%s] ==> ", v);
	final long startTime = System.currentTimeMillis ();
	final Object value = interpreter.eval (form);
	for (int i = 1; i < repeatCount; i++)
	{
	    interpreter.eval (form);
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
	buffer.append (">");
	return buffer.toString ();
    }
}
