
package lisp.lang;

import java.io.IOException;

public interface LispStream
{
    /**
     * Should this stream throw an error when EOF is read.
     *
     * @param eofThrows
     */
    default public void setEofThrows (final boolean eofThrows)
    {

    }

    /** Has this stream read all the input. */
    public boolean eof ();

    /** Close the stream. */
    public void close () throws IOException;

    /** Indicate that the reader has started a nested form */
    default public void startLevel ()
    {

    }

    /** Indicate that the reader has finished a nested form */
    default public void finishLevel ()
    {

    }

    /**
     * Peek at the next char without advancing the stream.
     *
     * @return The next char to be read.
     */
    public char peek () throws IOException;

    /**
     * Peek at the next char without advancing the stream.
     *
     * @param expected The character to check for.
     * @return True if the next char is the expected character. False otherwise. The character will
     *         still be remaining to be read in either case.
     */
    public boolean peek (char expected) throws IOException;

    /**
     * Peek at the second char without advancing the stream.
     *
     * @param expected The character to check for.
     * @return True if the next char is the expected character. False otherwise. The character will
     *         still be remaining to be read in either case.
     */
    public boolean peek2 (char expected) throws IOException;

    /** Read one character advancing the stream. */
    public char read () throws IOException;

    /**
     * Read one character advancing the stream.
     *
     * @param expected The character to check for.
     * @throws IllegalArgumentException If the next char is not the expected character.
     */
    public void read (char expected) throws IOException;

    /** Take the next char if it is expected, otherwise don't advance the stream. */
    public boolean tryChar (char expected) throws IOException;
}
