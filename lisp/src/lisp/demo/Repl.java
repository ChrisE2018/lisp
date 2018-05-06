
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
	final LispStream stream = new LispStream (System.in);
	repl.toplevel (stream);
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
		e.getCause ().printStackTrace ();
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
	System.out.printf ("[%s] ==> ", v);
	final Object value = interpreter.eval (form);
	v.setValue (value);
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
