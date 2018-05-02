
package lisp;

import java.io.*;

/** Wrapper around an InputStream for use by the Lisp reader. */
public class LispStream
{
    private final InputStream stream;

    private boolean eof = false;

    public LispStream (final InputStream stream)
    {
	this.stream = stream;
    }

    public boolean eof ()
    {
	return eof;
    }

    /** Peek at the next char without advancing the stream. */
    public char peek () throws IOException
    {
	stream.mark (1);
	final int result = stream.read ();
	if (result == -1)
	{
	    eof = true;
	}
	stream.reset ();
	return (char)result;
    }

    /** Peek at the next char without advancing the stream. */
    public boolean peek (final char expected) throws IOException
    {
	stream.mark (1);
	final int result = stream.read ();
	if (result == -1)
	{
	    eof = true;
	}
	stream.reset ();
	return (char)result == expected;
    }

    /** Peek at the second char without advancing the stream. */
    public boolean peek2 (final char expected) throws IOException
    {
	stream.mark (2);
	stream.read ();
	final int result = stream.read ();
	if (result == -1)
	{
	    eof = true;
	}
	stream.reset ();
	return (char)result == expected;
    }

    /** Read one character advancing the stream. */
    public char read () throws IOException
    {
	final int result = stream.read ();
	if (result == -1)
	{
	    eof = true;
	}
	return (char)result;
    }

    /** Read one character advancing the stream. */
    public void read (final char expected) throws IOException
    {
	final int result = stream.read ();
	if (result == -1)
	{
	    eof = true;
	}
	if (result != expected)
	{
	    throw new IllegalArgumentException ("Expected '" + expected + "' but found '" + (char)result + '"');
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	if (eof)
	{
	    buffer.append (" EOF");
	}
	buffer.append (" ");
	buffer.append (stream);
	buffer.append (">");
	return buffer.toString ();
    }
}
