
package lisp;

import java.io.IOException;

public class Parsing
{
    private static final char EXCLAMATION = '!';
    private static final char DOUBLE_QUOTE = '"';
    private static final char SINGLE_QUOTE = '\'';
    private static final char OPEN_PAREN = '(';
    private static final char OPEN_BRACE = '{';
    private static final char OPEN_BRACKET = '[';
    private static final char CLOSE_PAREN = ')';
    private static final char CLOSE_BRACE = '}';
    private static final char CLOSE_BRACKET = ']';

    /**
     * Map from list type to concrete class. Don't use angle brackets so they are available for
     * comparison operators.
     */
    private final Character[][] LIST_CLASSES =
	{
	 {OPEN_PAREN, CLOSE_PAREN},
	 {OPEN_BRACE, CLOSE_BRACE},
	 {OPEN_BRACKET, CLOSE_BRACKET}};

    /**
     * Characters that are replaced by a list starting with a specific symbol. <br>
     * [TODO] This should be a map
     */
    private final Object[][] SINGLE_CHAR_FORMS =
	{
	 {new Character (SINGLE_QUOTE), PackageFactory.getSystemPackage ().intern ("quote")},
	 {new Character (EXCLAMATION), PackageFactory.getSystemPackage ().intern ("not")}};

    private final CommentReader commentReader = new CommentReader ();

    /** Lists start with the default open char unless explicitly created otherwise. */
    public char getDefaultOpenChar ()
    {
	return OPEN_PAREN;
    }

    /** Lists finish with the default open char unless explicitly created otherwise. */
    public char getDefaultCloseChar ()
    {
	return CLOSE_PAREN;
    }

    /** Skip blanks and comments from an input stream. */
    public void skipBlanks (final LispStream in) throws IOException
    {
	commentReader.skipBlanks (in);
    }

    /**
     * Test for atom characters. [TODO] Make a syntax table.
     *
     * @param chr
     * @return
     */
    public boolean isAtomChar (final char chr)
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

    public LispList getParenList (final char open)
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

    public LispList getWrapperList (final char chr)
    {
	for (final Object[] slot : SINGLE_CHAR_FORMS)
	{
	    final Character ch = (Character)slot[0];
	    if (chr == ch)
	    {
		final Symbol wrapperSymbol = (Symbol)slot[1];
		final LispList result = new LispList ();
		result.add (wrapperSymbol);
		return result;
	    }
	}
	return null;
    }

    public Character getWrapperQuote (final Symbol symbol)
    {
	for (final Object[] slot : SINGLE_CHAR_FORMS)
	{
	    final Symbol wrapperSymbol = (Symbol)slot[1];
	    if (wrapperSymbol == symbol)
	    {
		final Character ch = (Character)slot[0];
		return ch;
	    }
	}
	return null;
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
