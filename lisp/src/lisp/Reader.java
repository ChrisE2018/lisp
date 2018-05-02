
package lisp;

import java.io.IOException;

/**
 * Read list structure from source text. <br>
 * [TODO] Make brace lists read as a map. <br>
 *
 * @author cre
 */
public class Reader
{
    private static final char EXCLAMATION = '!';
    private static final char SINGLE_QUOTE = '\'';
    private static final char DOUBLE_QUOTE = '"';
    private static final char OPEN_PAREN = '(';
    private static final char OPEN_BRACE = '{';
    private static final char OPEN_BRACKET = '[';
    private static final char CLOSE_PAREN = ')';
    private static final char CLOSE_BRACE = '}';
    private static final char CLOSE_BRACKET = ']';

    private final Package systemPackage = PackageFactory.getSystemPackage ();

    /**
     * Map from list type to concrete class. Don't use angle brackets so they are available for
     * comparison operators.
     */
    private final Character[][] LIST_CLASSES =
	{
	 {OPEN_PAREN, CLOSE_PAREN},
	 {OPEN_BRACE, CLOSE_BRACE},
	 {OPEN_BRACKET, CLOSE_BRACKET}};

    /** Characters that are replaced by a list starting with a specific symbol. */
    private final Object[][] SINGLE_CHAR_FORMS =
	{
	 {new Character (SINGLE_QUOTE), systemPackage.intern ("quote")},
	 {new Character (EXCLAMATION), systemPackage.intern ("not")}};

    private final CommentReader commentReader = new CommentReader ();

    public Object read (final LispStream in, final Package pkg) throws Exception
    {
	commentReader.skipBlanks (in);
	final char chr = in.peek ();
	if (chr == DOUBLE_QUOTE)
	{
	    return readString (in);
	}
	final LispList listResult = getParenList (chr);
	if (listResult != null)
	{
	    in.read ();
	    return readList (in, pkg, listResult);
	}
	// Handle single character form wrappers.
	// [TODO] These are not reversed properly on printing.
	for (final Object[] slot : SINGLE_CHAR_FORMS)
	{
	    final Character ch = (Character)slot[0];
	    if (chr == ch)
	    {
		in.read (ch); // Discard quote character
		final Symbol symbol = (Symbol)slot[1];
		final Object form = read (in, pkg);
		final LispList result = new LispList ();
		result.add (symbol);
		result.add (form);
		return result;
	    }
	}
	if (in.eof ())
	{
	    return null;
	}
	return readAtom (in, pkg);
    }

    private Object readList (final LispStream in, final Package pkg, final LispList result) throws IOException, Exception
    {
	final char close = result.getCloseChar ();
	while (!in.peek (close))
	{
	    final Object element = read (in, pkg);
	    result.add (element);
	    // [TODO] Handle colon and comma as distinct types of separator
	    // [TODO] Colon separated elements should be collected as a map.
	    commentReader.skipBlanks (in);
	}
	in.read ();
	return result;
    }

    private LispList getParenList (final char open)
    {
	for (final Character[] slot : LIST_CLASSES)
	{
	    final Character openChar = slot[0];
	    if (openChar.charValue () == open)
	    {
		final Character closeChar = slot[1];
		final LispList result = new LispList (openChar, closeChar);
		return result;
	    }
	}
	return null;
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
	return buffer.toString ();
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
     * Test for atom characters. [TODO] Make a syntax table.
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

    public static void printElement (final StringBuilder buffer, final Object element)
    {
	if (element instanceof String)
	{
	    buffer.append ('"');
	    // [TODO] Slashify
	    buffer.append (element);
	    buffer.append ('"');
	}
	else
	{
	    buffer.append (element);
	}
    }
}
