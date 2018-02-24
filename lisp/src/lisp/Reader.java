
package lisp;

import java.io.*;

public class Reader
{
    private static final char DOUBLE_QUOTE = '"';
    private static final char OPEN_PAREN = '(';
    private static final char OPEN_BRACE = '{';
    private static final char OPEN_BRACKET = '[';
    private static final char CLOSE_PAREN = ')';
    private static final char CLOSE_BRACE = '}';
    private static final char CLOSE_BRACKET = ']';

    private final CommentReader commentReader = new CommentReader ();

    public Lisp read (final InputStream in) throws IOException
    {
	commentReader.skipBlanks (in);
	final char chr = peek (in);
	if (chr == OPEN_PAREN || chr == OPEN_BRACE || chr == OPEN_BRACKET)
	{
	    return readList (in);
	}
	if (chr == DOUBLE_QUOTE)
	{
	    return readString (in);
	}
	return readAtom (in);
    }

    private Lisp readList (final InputStream in) throws IOException
    {
	final LispList result = getParenList ((char)in.read ());
	final ListKind listKind = result.getListKind ();
	final char close = listKind.getCloseChar ();
	char chr = peek (in);
	while (chr != close)
	{
	    final Lisp element = read (in);
	    result.add (element);
	    commentReader.skipBlanks (in);
	    chr = peek (in);
	}
	in.read ();
	return result;
    }

    private LispList getParenList (final char open)
    {
	switch (open)
	{
	    case OPEN_PAREN:
	    {
		return new LispParenList ();
	    }
	    case OPEN_BRACE:
	    {
		return new LispBraceList ();
	    }
	    case OPEN_BRACKET:
	    {
		return new LispBracketList ();
	    }
	    default:
	    {
		throw new IllegalArgumentException ("Open paren required");
	    }
	}
    }

    private Lisp readString (final InputStream in) throws IOException
    {
	int input = in.read ();
	input = in.read (); // Discard double quote
	final StringBuilder buffer = new StringBuilder ();
	// Need to handle embedded slashes
	while ((char)input != DOUBLE_QUOTE)
	{
	    buffer.append ((char)input);
	    input = in.read ();
	}
	return new StringAtom (buffer.toString ());
    }

    private Lisp readAtom (final InputStream in) throws IOException
    {
	final StringBuilder buffer = new StringBuilder ();
	while (isAtomChar (peek (in)))
	{
	    buffer.append ((char)in.read ());
	}
	final String s = buffer.toString ();
	// Try parsing an integer
	try
	{
	    final int value = Integer.parseInt (s);
	    return new IntAtom (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Try parsing a double
	try
	{
	    final double value = Double.parseDouble (s);
	    return new DoubleAtom (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Not a number. Return a symbol.
	// [TODO] Intern in the current package.
	return new Symbol (s);
    }

    /**
     * Test for atom characters. [TODO] Make a table.
     *
     * @param chr
     * @return
     */
    private boolean isAtomChar (final char chr)
    {
	if (Character.isWhitespace (chr))
	{
	    return false;
	}
	switch (chr)
	{
	    case DOUBLE_QUOTE:
	    case OPEN_PAREN:
	    case OPEN_BRACE:
	    case OPEN_BRACKET:
	    case CLOSE_PAREN:
	    case CLOSE_BRACE:
	    case CLOSE_BRACKET:
	    {
		return false;
	    }
	}
	return true;
    }

    private char peek (final InputStream in) throws IOException
    {
	in.mark (1);
	final int input = in.read ();
	in.reset ();
	return (char)input;
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
}
