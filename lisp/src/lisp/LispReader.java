
package lisp;

import java.io.IOException;
import java.util.*;

/**
 * Read list structure from source text.
 *
 * @author cre
 */
public class LispReader
{
    private static Map<Thread, LispReader> threadReaders = new HashMap<Thread, LispReader> ();
    private static final LispReader defaultLispReader = new LispReader ();

    public static LispReader getLispThreadReader ()
    {
	final Thread thread = Thread.currentThread ();
	LispReader result = threadReaders.get (thread);
	if (result == null)
	{
	    result = defaultLispReader;
	}
	return result;
    }

    public static Object withLispThreadReader (final LispReader lispReader, final ThrowingSupplier<Object> supplier)
            throws Exception
    {
	Object result = null;
	final Thread thread = Thread.currentThread ();
	final LispReader oldReader = threadReaders.get (thread);
	try
	{
	    threadReaders.put (thread, lispReader);
	    result = supplier.get ();
	}
	finally
	{
	    threadReaders.put (thread, oldReader);
	}
	return result;
    }

    private final List<Symbol> importedSymbols = new ArrayList<Symbol> ();
    private final List<Package> importedPackages = new ArrayList<Package> ();
    private Package currentPackage;

    private final Symbol theSymbol;

    public LispReader ()
    {
	importPackage (PackageFactory.getSystemPackage ());
	currentPackage = PackageFactory.getDefaultPackage ();
	theSymbol = PackageFactory.getSystemPackage ().internSymbol ("the");
    }

    public void importSymbol (final Symbol symbol)
    {
	if (!importedSymbols.contains (symbol))
	{
	    importedSymbols.add (symbol);
	}
    }

    public void importPackage (final Package pkg)
    {
	if (!importedPackages.contains (pkg))
	{
	    importedPackages.add (pkg);
	}
    }

    public Package getCurrentPackage ()
    {
	return currentPackage;
    }

    public void setCurrentPackage (final Package currentPackage)
    {
	this.currentPackage = currentPackage;
    }

    /**
     * Read a single form from the input stream using the current package.
     *
     * @param in The input stream.
     * @return The form read.
     * @throws IOException
     */
    public Object read (final LispStream in) throws IOException
    {
	return read (in, currentPackage);
    }

    /**
     * Read a single form from the input stream.
     *
     * @param in The input stream.
     * @param pkg The default package for symbols.
     * @return The form read.
     * @throws IOException
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
	final LispList mapResult = parsing.getMapResult (chr);
	if (mapResult != null)
	{
	    in.read ();
	    return readMap (in, pkg, mapResult);
	}
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
	    parsing.skipBlanks (in);
	    if (in.tryChar (parsing.getTheMarker ()))
	    {
		final Object e2 = read (in, pkg);
		final LispList the = new LispList ();
		the.add (theSymbol);
		the.add (element);
		the.add (e2);
		result.add (the);
	    }
	    else
	    {
		result.add (element);
	    }
	    parsing.skipBlanks (in);
	}
	in.read ();
	return result;
    }

    /** Read a map where comma separates entries. */
    private Object readMap (final LispStream in, final Package pkg, final LispList result) throws IOException
    {
	final Parsing parsing = pkg.getParsing ();
	final char close = result.getCloseChar ();
	final char separator = parsing.getMapSeparator ();
	final char theMarker = parsing.getTheMarker ();

	LispList element = null;
	while (true)
	{
	    parsing.skipBlanks (in);
	    final char p = in.peek (); // For debug
	    if (p == separator)
	    {
		in.read (p);
		if (element == null)
		{
		    element = new LispList ();
		}
		result.add (element);
		element = new LispList ();
	    }
	    else if (p == close)
	    {
		in.read (p);
		if (element != null)
		{
		    result.add (element);
		}
		return result;
	    }
	    else if (p == -1)
	    {
		return result;
	    }
	    else
	    {
		final Object key = read (in, pkg);
		parsing.skipBlanks (in);
		if (element == null)
		{
		    element = new LispList ();
		}
		if (in.tryChar (theMarker))
		{
		    final LispList the = new LispList ();
		    the.add (theSymbol);
		    the.add (key);
		    the.add (read (in, pkg));
		    element.add (the);
		}
		else
		{
		    element.add (key);
		}
	    }
	}
    }

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
	return readSymbol (pkg, s);
    }

    public Symbol readSymbol (final Package pkg, final String name)
    {
	final int pos = name.indexOf (Symbol.PACKAGE_SEPARATOR);
	if (pos >= 0)
	{
	    // Process package prefix
	    final String packageName = name.substring (0, pos);
	    final Package p = PackageFactory.getPackage (packageName);

	    final String symbolName = name.substring (pos + 1);
	    final Symbol result = p.internSymbol (symbolName);
	    return result;
	}
	final Symbol result = findImportedSymbol (name);
	if (result != null)
	{
	    return result;
	}
	return pkg.internSymbol (name);
    }

    /** Lookup a name to determine if it represents an imported symbol. */
    public Symbol findImportedSymbol (final String name)
    {
	for (final Symbol symbol : importedSymbols)
	{
	    if (symbol.is (name))
	    {
		return symbol;
	    }
	}
	for (final Package p : importedPackages)
	{
	    final Symbol result = p.findSymbol (name);
	    if (result != null)
	    {
		return result;
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

	buffer.append (" ");
	buffer.append (System.identityHashCode (this));

	buffer.append (" ");
	buffer.append (currentPackage);

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
