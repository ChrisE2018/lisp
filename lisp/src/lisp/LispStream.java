
package lisp;

import java.io.*;

/** Wrapper around an InputStream for use by the Lisp reader. */
public class LispStream
{
    private final InputStream stream;

    public LispStream (final InputStream stream)
    {
	this.stream = stream;
    }

    /** Peek at the next char without advancing the stream. */
    public char peek () throws IOException
    {
	stream.mark (1);
	final char result = (char)stream.read ();
	stream.reset ();
	return result;
    }

    /** Peek at the next two chars without advancing the stream. */
    public String peek2 () throws IOException
    {
	stream.mark (2);
	final char data[] =
	    {(char)stream.read (), (char)stream.read ()};
	final String result = new String (data);
	stream.reset ();
	return result;
    }

    /** Read one character advancing the stream. */
    public char read () throws IOException
    {
	final char result = (char)stream.read ();
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (stream);
	buffer.append (">");
	return buffer.toString ();
    }
}
