
package lisp.lang;

import java.io.*;
import java.nio.charset.StandardCharsets;

/**
 * Wrapper around an InputStream for use by the Lisp reader. This also supports reading from text,
 * since that is a one line constructor addition.
 */
public class LispInputStream implements LispStream
{
    private final InputStream stream;

    private boolean eof = false;

    private boolean eofThrows = true;

    public LispInputStream (final InputStream stream)
    {
	this.stream = stream;
    }

    public LispInputStream (final String text)
    {
	stream = new ByteArrayInputStream (text.getBytes (StandardCharsets.UTF_8));
    }

    /*
     * Should this stream throw an error when EOF is read. (non-Javadoc)
     * @see lisp.LispStream#setEofThrows(boolean)
     */
    @Override
    public void setEofThrows (final boolean eofThrows)
    {
	this.eofThrows = eofThrows;
    }

    /**
     * Has this stream read all the input.
     *
     * @see lisp.lang.LispStream#eof()
     */
    @Override
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

    /*
     * Close the stream.
     * @see lisp.LispStream#close()
     */
    @Override
    public void close () throws IOException
    {
	stream.close ();
    }

    /*
     * Peek at the next char without advancing the stream.
     * @return The next char to be read.
     * @see lisp.LispStream#peek()
     */
    @Override
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

    /*
     * Peek at the next char without advancing the stream.
     * @param expected The character to check for.
     * @return True if the next char is the expected character. False otherwise. The character will
     * still be remaining to be read in either case.
     * @see lisp.LispStream#peek(char)
     */
    @Override
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

    /*
     * Peek at the second char without advancing the stream.
     * @param expected The character to check for.
     * @return True if the next char is the expected character. False otherwise. The character will
     * still be remaining to be read in either case.
     * @see lisp.LispStream#peek2(char)
     */
    @Override
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

    /*
     * Read one character advancing the stream.
     * @see lisp.LispStream#read()
     */
    @Override
    public char read () throws IOException
    {
	final int result = stream.read ();
	if (result == -1)
	{
	    markEof ();
	}
	return (char)result;
    }

    /*
     * Read one character advancing the stream.
     * @param expected The character to check for.
     * @throws IllegalArgumentException If the next char is not the expected character.
     * @see lisp.LispStream#read(char)
     */
    @Override
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

    /*
     * Take the next char if it is expected, otherwise don't advance the stream. Combines peek and
     * read.
     * @param expected The character to check for.
     * @return If the next char is the expected character, advance the stream and return true.
     * Otherwise do not advance the stream and return false otherwise.
     * @see lisp.LispStream#tryChar(char)
     */
    @Override
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
