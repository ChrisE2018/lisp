
package lisp;

import java.io.IOException;

/**
 * Control over parsing syntax is collected into the Parsing object. Various constants in this class
 * could be made customizable to modify the parsing behavior. That is a project for another day.
 */
public class Parsing
{
    private static final char EXCLAMATION = '!';
    private static final char DOUBLE_QUOTE = '"';
    private static final char SINGLE_QUOTE = '\'';
    private static final char COMMA = ',';
    private static final char OPEN_PAREN = '(';
    private static final char OPEN_BRACE = '{';
    private static final char OPEN_BRACKET = '[';
    private static final char CLOSE_PAREN = ')';
    private static final char CLOSE_BRACE = '}';
    private static final char CLOSE_BRACKET = ']';

    private static final char COLON = ':';

    /**
     * Map from list type to concrete class. Don't use angle brackets so they are available for
     * comparison operators.
     */
    private static final Character[][] LIST_DELIMITERS = {{OPEN_PAREN, CLOSE_PAREN}, {OPEN_BRACE, CLOSE_BRACE},
                                                          {OPEN_BRACKET, CLOSE_BRACKET}};

    private static final Package SYSTEM_PACKAGE = PackageFactory.getSystemPackage ();

    private static final Object[][] WRAPPER_SYMBOLS = {{SINGLE_QUOTE, SYSTEM_PACKAGE.internSymbol ("quote")},
                                                       {EXCLAMATION, SYSTEM_PACKAGE.internSymbol ("not")}};

    private static Parsing DEFAULT_PARSING = null;

    /**
     * Characters that are replaced by a list starting with a specific symbol.
     *
     * @see https://stackoverflow.com/questions/1670038/does-java-have-a-hashmap-with-reverse-lookup
     */
    private final SimpleBiMap<Character, Symbol> wrapperSymbols = new SimpleBiMap<Character, Symbol> (WRAPPER_SYMBOLS);

    private final CommentReader commentReader = new CommentReader ();

    public static Parsing getDefaultParsing ()
    {
	if (DEFAULT_PARSING == null)
	{
	    DEFAULT_PARSING = new Parsing ();
	}
	return DEFAULT_PARSING;
    }

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
     * Test for atom characters. CONSIDER Make a syntax table.
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
	    case SINGLE_QUOTE:
	    case OPEN_PAREN:
	    case OPEN_BRACE:
	    case OPEN_BRACKET:
	    case CLOSE_PAREN:
	    case CLOSE_BRACE:
	    case CLOSE_BRACKET:
	    case COMMA:
	    case COLON:
	    {
		return false;
	    }
	}
	return true;
    }

    public LispList getMapResult (final char open)
    {
	if (open == OPEN_BRACE)
	{
	    // Can't return a brace list or print/read would not round trip.
	    // Special logic in print could turn nested lists into brace lists, but that would not
	    // be an exact reverse either.
	    return new LispList (OPEN_BRACE, CLOSE_BRACE);
	}
	return null;
    }

    public char getMapOpen ()
    {
	return OPEN_BRACE;
    }

    public char getMapClose ()
    {
	return CLOSE_BRACE;
    }

    public char getMapSeparator ()
    {
	return COMMA;
    }

    public LispList getParenList (final char open)
    {
	for (final Character[] slot : LIST_DELIMITERS)
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
	final Symbol wrapperSymbol = wrapperSymbols.get (chr);
	if (wrapperSymbol != null)
	{
	    final LispList result = new LispList ();
	    result.add (wrapperSymbol);
	    return result;
	}
	return null;
    }

    public Character getWrapperQuote (final Symbol symbol)
    {
	return wrapperSymbols.getKey (symbol);
    }

    public char getStringDelimiter ()
    {
	return DOUBLE_QUOTE;
    }

    public char getTheMarker ()
    {
	return COLON;
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
