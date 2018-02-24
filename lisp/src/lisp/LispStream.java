
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

    /** Peek at the next char without advancing the stream. */
    public boolean peek (final char expected) throws IOException
    {
	stream.mark (1);
	final char result = (char)stream.read ();
	stream.reset ();
	return result == expected;
    }

    /** Peek at the second char without advancing the stream. */
    public boolean peek2 (final char expected) throws IOException
    {
	stream.mark (2);
	stream.read ();
	final char result = (char)stream.read ();
	stream.reset ();
	return result == expected;
    }

    /** Read one character advancing the stream. */
    public char read () throws IOException
    {
	final char result = (char)stream.read ();
	return result;
    }

    /** Read one character advancing the stream. */
    public void read (final char expected) throws IOException
    {
	final char result = (char)stream.read ();
	if (result != expected)
	{
	    throw new IllegalArgumentException ("Expected '" + expected + "'");
	}
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
