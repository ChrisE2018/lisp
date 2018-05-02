
package lisp.demo;

import lisp.*;
import lisp.Package;
import lisp.eval.*;

/** Simple toplevel loop that reads a lisp form, evaluates it and prints the result. */
public class Repl
{
    private final LispReader reader;

    private final Interpreter interpreter;

    public static void main (final String[] args) throws NoSuchMethodException, SecurityException
    {
	final Repl repl = new Repl ();
	repl.toplevel ();
    }

    /** Constructor for demo application. */
    private Repl () throws NoSuchMethodException, SecurityException
    {
	Primitives.initialize ();
	interpreter = new Interpreter ();
	reader = new LispReader ();
    }

    /** Constructor to use an interpreter built elsewhere. */
    public Repl (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
	reader = new LispReader ();
    }

    public void toplevel ()
    {
	final LispStream stream = new LispStream (System.in);
	while (true)
	{
	    try
	    {
		Thread.sleep (100);
		rep (stream);
	    }
	    catch (final Throwable e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    private void rep (final LispStream stream) throws Exception
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	Object form = null;
	try
	{
	    form = reader.read (stream, pkg);
	}
	catch (final Throwable e)
	{
	    try
	    {
		// Read to a newline character
		while (stream.read () != '\n')
		{

		}
	    }
	    catch (final Throwable ex)
	    {
		System.out.printf ("[Error recovering from error: %s]", ex);
	    }
	    return;
	}
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	final StringBuilder buffer = new StringBuilder ();
	// buffer.append (form.toString ());
	// System.out.println (buffer);
	System.out.print (" ==> ");
	final Object value = interpreter.eval (form);
	buffer.setLength (0);
	if (value == null)
	{
	    buffer.append ("null");
	}
	else
	{
	    LispReader.printElement (buffer, value);
	}
	System.out.println (buffer);
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
