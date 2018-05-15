
package lisp;

import java.io.*;
import java.net.URL;

import lisp.eval.Interpreter;

/** Class to read a lisp file and evaluate the forms in the file. */
public class FileReader
{
    private boolean trace = false;

    public void setTrace (final boolean trace)
    {
	this.trace = trace;
    }

    public Object read (final Interpreter interpreter, final Package pkg, final String pathname) throws Exception
    {
	return read (interpreter, pkg, new File (pathname));
    }

    public Object read (final Interpreter interpreter, final String pathname) throws Exception
    {
	return read (interpreter, new File (pathname));
    }

    public Object read (final Interpreter interpreter, final File file) throws Exception
    {
	return read (interpreter, PackageFactory.getDefaultPackage (), file);
    }

    public Object read (final Interpreter interpreter, final URL url) throws Exception
    {
	return read (interpreter, PackageFactory.getDefaultPackage (), url);
    }

    public Object read (final Interpreter interpreter, final Package pkg, final URL url) throws Exception
    {
	Object result = null;
	try (InputStream in = url.openStream ())
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispStream (b);
	    result = read (interpreter, pkg, stream);
	}
	return result;
    }

    public Object read (final Interpreter interpreter, final Package pkg, final File file) throws Exception
    {
	Object result = null;
	// final FileInputStream in = new FileInputStream (file);
	try (FileInputStream in = new FileInputStream (file))
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispStream (b);
	    result = read (interpreter, pkg, stream);
	}
	// finally
	// {
	// in.close ();
	// }
	return result;
    }

    public Object read (final Interpreter interpreter, final Package pkg, final LispStream stream) throws Exception
    {
	Object result = null;
	try
	{
	    final LispReader reader = new LispReader ();
	    while (!stream.eof ())
	    {
		final Object form = reader.read (stream, pkg);
		if (form != null)
		{
		    result = interpreter.eval (form);
		    if (trace)
		    {
			System.out.printf ("%s => %s %n", form, result);
		    }
		}
	    }
	}
	finally
	{
	    stream.close ();
	}
	return result;
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

    public static void main (final String[] args) throws Exception
    {
	// Primitives.initialize ();
	final Interpreter interpreter = new Interpreter ();
	final FileReader fr = new FileReader ();
	final File file = new File ("sample.lisp");
	System.out.printf ("File: %s %n", file);
	System.out.printf ("Package: %s %n", PackageFactory.getDefaultPackage ());
	final Object result = fr.read (interpreter, file);
	System.out.printf ("Result: %s %n", result);
    }
}
