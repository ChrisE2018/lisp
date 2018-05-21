
package lisp;

import java.io.*;
import java.net.URL;

import lisp.eval.*;

/** Class to read a lisp file and evaluate the forms in the file. */
public class FileReader
{
    private boolean trace = false;

    public void setTrace (final boolean trace)
    {
	this.trace = trace;
    }

    // [TODO] All of these should return boolean true if the file is loaded, false if not found or
    // otherwise not loaded.
    public Object read (final LexicalContext context, final Package pkg, final String pathname) throws Exception
    {
	return read (context, pkg, new File (pathname));
    }

    public Object read (final LexicalContext context, final String pathname) throws Exception
    {
	return read (context, new File (pathname));
    }

    public Object read (final LexicalContext context, final File file) throws Exception
    {
	return read (context, PackageFactory.getDefaultPackage (), file);
    }

    public Object read (final LexicalContext context, final URL url) throws Exception
    {
	return read (context, PackageFactory.getDefaultPackage (), url);
    }

    public Object read (final LexicalContext context, final Package pkg, final URL url) throws Exception
    {
	Object result = null;
	try (InputStream in = url.openStream ())
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispStream (b);
	    result = read (context, pkg, stream);
	}
	return result;
    }

    public Object read (final LexicalContext context, final Package pkg, final File file) throws Exception
    {
	Object result = null;
	try (FileInputStream in = new FileInputStream (file))
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispStream (b);
	    try
	    {
		result = read (context, pkg, stream);
	    }
	    catch (final EOFException e)
	    {

	    }
	}
	return result;
    }

    public Object read (final LexicalContext context, final Package pkg, final LispStream stream) throws Exception
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
		    result = context.eval (form);
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
	final LexicalContext context = new LexicalContext (interpreter);
	final FileReader fr = new FileReader ();
	final File file = new File ("sample.lisp");
	System.out.printf ("File: %s %n", file);
	System.out.printf ("Package: %s %n", PackageFactory.getDefaultPackage ());
	final Object result = fr.read (context, file);
	System.out.printf ("Result: %s %n", result);
    }
}
