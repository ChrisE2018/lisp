
package plan;

import java.io.*;

import lisp.*;
import lisp.Package;
import lisp.LispReader;
import lisp.demo.Repl;
import lisp.eval.Interpreter;

public class Demo1
{
    public static void main (final String[] args) throws SecurityException, NoSuchMethodException
    {
	PlanFunctions.initialize ();
	final Interpreter interpreter = new Interpreter ();
	final Demo1 d = new Demo1 ();
	try
	{
	    d.execute (interpreter);
	}
	catch (final Exception e)
	{
	    e.printStackTrace ();
	}
	try
	{
	    final Repl repl = new Repl (interpreter);
	    repl.toplevel ();
	}
	catch (final Exception e)
	{
	    e.printStackTrace ();
	}
    }

    private void execute (final Interpreter interpreter) throws Exception
    {
	final Package pkg = PackageFactory.getPackage ("user");
	final File file = new File ("examples.txt");
	System.out.printf ("File: %s %n", file);
	final FileInputStream in = new FileInputStream (file);
	final BufferedInputStream b = new BufferedInputStream (in);
	final LispStream stream = new LispStream (b);
	final LispReader reader = new LispReader ();
	for (int i = 0; i < 20 && !stream.eof (); i++)
	{
	    final Object form = reader.read (stream, pkg);
	    if (form != null)
	    {
		final StringBuilder buffer = new StringBuilder ();
		buffer.append (form);
		System.out.printf ("%s ", buffer);
		final Object value = interpreter.eval (form);
		buffer.setLength (0);
		if (value == null)
		{
		    buffer.append ("[null]");
		}
		else
		{
		    LispReader.printElement (buffer, value);
		}
		System.out.printf ("=> %s %n%n", buffer);
	    }
	}
	if (stream.eof ())
	{
	    System.out.printf ("[EOF]%n");
	}
	in.close ();
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
