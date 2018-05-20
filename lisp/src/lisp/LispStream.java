
package lisp;

import java.io.*;
import java.nio.charset.StandardCharsets;

/** Wrapper around an InputStream for use by the Lisp reader. */
public class LispStream
{
    private final InputStream stream;

    private boolean eof = false;

    private boolean eofThrows = true;

    public LispStream (final InputStream stream)
    {
	this.stream = stream;
    }

    public LispStream (final String text)
    {
	stream = new ByteArrayInputStream (text.getBytes (StandardCharsets.UTF_8));
    }

    public void setEofThrows (final boolean eofThrows)
    {
	this.eofThrows = eofThrows;
    }

    public boolean eof ()
    {
	return eof;
    }

    private void markEof () throws EOFException
    {
	eof = true;
	if (eofThrows)
	{
	    throw new EOFException ();
	}
    }

    /** Take the next char if it is expected, otherwise don't advance the stream. */
    public boolean tryChar (final char expected) throws IOException
    {
	stream.mark (1);
	final int result = stream.read ();
	if (result == -1)
	{
	    markEof ();
	}
	if ((char)result == expected)
	{
	    return true;
	}
	else
	{
	    stream.reset ();
	    return false;
	}
    }

    /** Peek at the next char without advancing the stream. */
    public char peek () throws IOException
    {
	stream.mark (1);
	final int result = stream.read ();
	if (result == -1)
	{
	    markEof ();
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
	    markEof ();
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
	    markEof ();
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
	    markEof ();
	}
	return (char)result;
    }

    /** Read one character advancing the stream. */
    public void read (final char expected) throws IOException
    {
	final int result = stream.read ();
	if (result == -1)
	{
	    markEof ();
	}
	if (result != expected)
	{
	    throw new IllegalArgumentException ("Expected '" + expected + "' but found '" + (char)result + '"');
	}
    }

    public void close () throws IOException
    {
	stream.close ();
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
