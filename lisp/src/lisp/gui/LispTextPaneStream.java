/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.gui;

import java.io.*;
import java.util.*;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;

import lisp.LispStream;

/**
 * @author cre
 */
public class LispTextPaneStream implements LispStream
{
    private static final boolean eofThrows = true;

    private final JTextPane textPane;
    private int offset;
    private final int limit;
    private boolean eof = false;

    /** Record of indentation level as each read level starts. */
    private final List<Integer> indentation = new ArrayList<Integer> ();

    public LispTextPaneStream (final JTextPane textPane, final int offset, final int limit)
    {
	this.textPane = textPane;
	this.offset = offset;
	this.limit = limit;
    }

    public int getIndentation ()
    {
	if (indentation.size () > 0)
	{
	    return indentation.get (indentation.size () - 1);
	}
	return 0;
    }

    /**
     * Has this stream read all the input.
     *
     * @see lisp.LispStream#eof()
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
	markEof ();
    }

    /** Indicate that the reader has started a nested form */
    public void startLevel ()
    {
	final int linePos = getLinePosition (offset);
	indentation.add (linePos);
    }

    private int getLinePosition (final int n)
    {
	try
	{
	    for (int i = n; i >= 0; i--)
	    {
		final String str = textPane.getText (i, 1);
		if (str.charAt (0) == '\n')
		{
		    return n - i;
		}
	    }
	}
	catch (final BadLocationException e)
	{
	}
	return -1;
    }

    /** Indicate that the reader has finished a nested form */
    public void finishLevel ()
    {
	indentation.remove (indentation.size () - 1);
    }

    /*
     * Peek at the next char without advancing the stream.
     * @return The next char to be read.
     * @see lisp.LispStream#peek()
     */
    @Override
    public char peek () throws IOException
    {
	if (offset < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset, 1);
		return s.charAt (0);
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
	}
	return (char)0;
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
	if (offset < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset, 1);
		return s.charAt (0) == expected;
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
	}
	return false;
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
	if (offset + 1 < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset + 1, 1);
		return s.charAt (0) == expected;
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
	}
	return false;
    }

    /*
     * Read one character advancing the stream.
     * @see lisp.LispStream#read()
     */
    @Override
    public char read () throws IOException
    {
	if (offset < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset, 1);
		offset++;
		return s.charAt (0);
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
	}
	return (char)0;
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
	if (offset < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset, 1);
		final char result = s.charAt (0);
		if (result != expected)
		{
		    throw new IllegalArgumentException ("Expected '" + expected + "' but found '" + result + '"');
		}
		offset++;
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
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
	if (offset < limit)
	{
	    try
	    {
		final String s = textPane.getText (offset, 1);
		final char result = s.charAt (0);
		if (result == expected)
		{
		    offset++;
		    return true;
		}
	    }
	    catch (final BadLocationException e)
	    {
	    }
	}
	else
	{
	    markEof ();
	}
	return false;
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
