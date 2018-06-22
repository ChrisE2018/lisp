
package lisp.lang;

import java.io.*;
import java.net.URL;
import java.util.logging.Logger;

import lisp.eval.LexicalContext;
import lisp.util.ThrowingSupplier;

/** Class to read a lisp file and evaluate the forms in the file. */
public class FileReader
{
    private static final Logger LOGGER = Logger.getLogger (FileReader.class.getName ());

    // All of these should return boolean true if the file is loaded, false or exception if there is
    // a problem

    public boolean read (final LexicalContext context, final String pathname) throws Exception
    {
	return read (context, new File (pathname));
    }

    public boolean read (final LexicalContext context, final URL url) throws Exception
    {
	try (InputStream in = url.openStream ())
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispInputStream (b);
	    return read (context, stream);
	}
    }

    public boolean read (final LexicalContext context, final File file) throws Exception
    {
	try (FileInputStream in = new FileInputStream (file))
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispInputStream (b);
	    try
	    {
		read (context, stream);
	    }
	    catch (final EOFException e)
	    {

	    }
	}
	return true;
    }

    private boolean read (final LexicalContext context, final LispStream stream) throws Exception
    {
	// This LispReader must be associated with the LispThread
	final LispReader reader = LispReader.getLispThreadReader ();
	LispReader.withLispThreadReader (reader, new ThrowingSupplier<Object> ()
	{
	    @Override
	    public Object get () throws Exception
	    {
		Object supplierResult = null;
		try
		{
		    while (!stream.eof ())
		    {
			final Object form = reader.read (stream);
			if (form != null)
			{
			    supplierResult = context.eval (form);
			    LOGGER.finer (String.format ("%s => %s", form, supplierResult));
			}
		    }
		}
		catch (final EOFException e)
		{

		}
		finally
		{
		    stream.close ();
		}
		return supplierResult;
	    }
	});
	return true;
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
