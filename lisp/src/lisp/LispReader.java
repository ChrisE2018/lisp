
package lisp;

import java.io.IOException;

/**
 * Read list structure from source text.
 *
 * @author cre
 */
public class LispReader
{
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
	if (chr == parsing.getStringDelimiter ())
	{
	    return readString (parsing.getStringDelimiter (), in);
	}
	// final Map<Object, Object> mapResult = parsing.getMapResult (chr);
	// if (mapResult != null)
	// {
	// in.read ();
	// return readMap (in, pkg, mapResult);
	// }
	final LispList listResult = parsing.getParenList (chr);
	if (listResult != null)
	{
	    in.read ();
	    return readList (in, pkg, listResult);
	}
	// Handle single character form wrappers.
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
	    parsing.skipBlanks (in);
	}
	in.read ();
	return result;
    }

    // /** Read a map using colon and comma to separate entries. */
    // private Object readMap (final LispStream in, final Package pkg, final Map<Object, Object>
    // mapResult) throws IOException
    // {
    // final Parsing parsing = pkg.getParsing ();
    // final char close = parsing.getMapClose ();
    // final char mapCombiner = parsing.getMapCombiner ();
    // final char mapSeparator = parsing.getMapSeparator ();
    // boolean done = in.peek (close);
    // while (!done)
    // {
    // final Object key = read (in, pkg);
    // parsing.skipBlanks (in);
    // in.read (mapCombiner);
    // final Object value = read (in, pkg);
    // mapResult.put (key, value);
    // parsing.skipBlanks (in);
    // if (in.peek (close))
    // {
    // done = true;
    // }
    // else
    // {
    // in.read (mapSeparator);
    // }
    // }
    // in.read ();
    // return mapResult;
    // }

    /**
     * Read a double quoted string as a java String.
     *
     * @param stringDelimiter
     */
    private Object readString (final char stringDelimiter, final LispStream in) throws IOException
    {
	in.read (stringDelimiter); // Discard double quote
	int input = in.read ();
	final StringBuilder buffer = new StringBuilder ();
	// Need to handle embedded slashes
	while ((char)input != stringDelimiter)
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
	if (s.length () == 0)
	{
	    throw new IOException ("Character " + (in.peek ()) + " cannot start an atom");
	}
	// Try parsing an integer
	try
	{
	    // [MAYBE] Create a table for small integers to save memory.
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
	    // Process package prefix
	    final String packageName = s.substring (0, pos);
	    final Package p = PackageFactory.getPackage (packageName);
	    if (s.charAt (pos + 1) == Symbol.PACKAGE_SEPARATOR)
	    {
		if (s.charAt (pos + 2) == Symbol.PACKAGE_SEPARATOR)
		{
		    // Lookup public external symbol, creating it if needed.
		    final String symbolName = s.substring (pos + 3);
		    return p.internPublic (symbolName);
		}
		// Lookup private internal symbol, creating it if needed.
		final String symbolName = s.substring (pos + 2);
		return p.internPrivate (symbolName);
	    }
	    else
	    {
		// Lookup public external symbol. Don't create the symbol if it does not already
		// exist.
		final String symbolName = s.substring (pos + 1);
		final Symbol result = p.findPublic (symbolName);
		if (result == null)
		{
		    throw new IOException ("Package " + packageName + " has no public symbol " + symbolName);
		}
		return result;
	    }
	}
	return pkg.internPrivate (s);
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
