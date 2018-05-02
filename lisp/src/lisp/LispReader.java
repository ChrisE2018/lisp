
package lisp;

import java.io.IOException;

/**
 * Read list structure from source text. <br>
 * [TODO] Make brace lists read as a map. <br>
 *
 * @author cre
 */
public class LispReader
{
    private static final char DOUBLE_QUOTE = '"';

    /**
     * Read a single form from the input stream.
     *
     * @param in The input stream.
     * @param pkg The default package for symbols.
     * @return The form read.
     * @throws IOException
     * @throws Exception
     */
    public Object read (final LispStream in, final Package pkg) throws IOException
    {
	final Parsing parsing = pkg.getParsing ();
	parsing.skipBlanks (in);
	final char chr = in.peek ();
	if (chr == DOUBLE_QUOTE)
	{
	    return readString (in);
	}
	final LispList listResult = parsing.getParenList (chr);
	if (listResult != null)
	{
	    in.read ();
	    return readList (in, pkg, listResult);
	}
	// Handle single character form wrappers.
	// [TODO] These are not reversed properly on printing.
	final LispList wrapper = parsing.getWrapperList (chr);
	if (wrapper != null)
	{
	    in.read (chr); // Discard quote character
	    final Object form = read (in, pkg);
	    wrapper.add (form);
	    return wrapper;
	}
	if (in.eof ())
	{
	    return null;
	}
	return readAtom (in, pkg);
    }

    private Object readList (final LispStream in, final Package pkg, final LispList result) throws IOException
    {
	final Parsing parsing = pkg.getParsing ();
	final char close = result.getCloseChar ();
	while (!in.peek (close))
	{
	    final Object element = read (in, pkg);
	    result.add (element);
	    // [TODO] Handle colon and comma as distinct types of separator
	    // [TODO] Colon separated elements should be collected as a map.
	    parsing.skipBlanks (in);
	}
	in.read ();
	return result;
    }

    /** Read a double quoted string as a java String. */
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

    /** Read a number or symbol. */
    private Object readAtom (final LispStream in, final Package pkg) throws IOException
    {
	final StringBuilder buffer = new StringBuilder ();
	final Parsing parsing = pkg.getParsing ();
	while (parsing.isAtomChar (in.peek ()))
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
