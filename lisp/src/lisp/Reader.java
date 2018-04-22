
package lisp;

import java.io.IOException;

public class Reader
{
    private static final char SINGLE_QUOTE = '\'';
    private static final char DOUBLE_QUOTE = '"';
    private static final char OPEN_PAREN = '(';
    private static final char OPEN_BRACE = '{';
    private static final char OPEN_BRACKET = '[';
    private static final char CLOSE_PAREN = ')';
    private static final char CLOSE_BRACE = '}';
    private static final char CLOSE_BRACKET = ']';

    private final Package systemPackage = PackageFactory.getSystemPackage ();

    private final CommentReader commentReader = new CommentReader ();

    public Object read (final LispStream in, final Package pkg) throws IOException
    {
	commentReader.skipBlanks (in);
	final char chr = in.peek ();
	if (chr == OPEN_PAREN || chr == OPEN_BRACE || chr == OPEN_BRACKET)
	{
	    return readList (in, pkg);
	}
	if (chr == DOUBLE_QUOTE)
	{
	    return readString (in);
	}
	if (chr == SINGLE_QUOTE)
	{
	    in.read (SINGLE_QUOTE); // Discard quote
	    final Object quote = systemPackage.intern ("quote");
	    final Object form = read (in, pkg);
	    final LispParenList result = new LispParenList ();
	    result.add (quote);
	    result.add (form);
	    return result;
	}
	if (in.eof ())
	{
	    return null;
	}
	return readAtom (in, pkg);
    }

    private Object readList (final LispStream in, final Package pkg) throws IOException
    {
	final LispList result = getParenList (in.read ());
	final ListKind listKind = result.getListKind ();
	final char close = listKind.getCloseChar ();
	while (!in.peek (close))
	{
	    final Object element = read (in, pkg);
	    result.add (element);
	    // [TODO] Handle colon and comma as distinct types of separatorGE
	    commentReader.skipBlanks (in);
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

    private Object readString (final LispStream in) throws IOException
    {
	in.read (DOUBLE_QUOTE); // Discard double quote
	int input = in.read ();
	final StringBuilder buffer = new StringBuilder ();
	// Need to handle embedded slashes
	while ((char)input != DOUBLE_QUOTE)
	{
	    buffer.append ((char)input);
	    input = in.read ();
	}
	return new StringAtom (buffer.toString ());
    }

    private Object readAtom (final LispStream in, final Package pkg) throws IOException
    {
	final StringBuilder buffer = new StringBuilder ();
	while (isAtomChar (in.peek ()))
	{
	    buffer.append (in.read ());
	}
	final String s = buffer.toString ();
	// Try parsing an integer
	try
	{
	    // [TODO] Create a table for small integers to save memory.
	    final int value = Integer.parseInt (s);
	    return new Integer (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Try parsing a double
	try
	{
	    final double value = Double.parseDouble (s);
	    return new Double (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Not a number. Return a symbol.
	final int pos = s.indexOf (Symbol.PACKAGE_SEPARATOR);
	if (pos >= 0)
	{
	    // Implement package prefix
	    final String packageName = s.substring (0, pos);
	    final String symbolName = s.substring (pos + 1);
	    final Package p = PackageFactory.getPackage (packageName);
	    return p.intern (symbolName);
	}
	return pkg.intern (s);
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
