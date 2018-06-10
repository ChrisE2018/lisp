
package lisp;

import java.io.IOException;
import java.lang.reflect.*;
import java.util.*;

import lisp.util.ThrowingSupplier;

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
    private final Symbol dotSymbol;

    public LispReader ()
    {
	importPackage (PackageFactory.getSystemPackage ());
	currentPackage = PackageFactory.getDefaultPackage ();
	theSymbol = PackageFactory.getSystemPackage ().internSymbol ("the");
	dotSymbol = PackageFactory.getSystemPackage ().internSymbol ("dot");
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
     * @throws ClassNotFoundException
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
     * @throws ClassNotFoundException
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
	if (chr == parsing.getDotMarker ())
	{
	    final LispList dotForm = new LispList ();
	    in.read (chr); // Discard dot character
	    dotForm.add (dotSymbol);
	    final Object form = read (in, pkg);
	    dotForm.add (form);
	    return dotForm;
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
	    readInsideList (in, pkg, result);
	    parsing.skipBlanks (in);
	}
	in.read ();
	return result;
    }

    private void readInsideList (final LispStream in, final Package pkg, final LispList result) throws IOException
    {
	final Parsing parsing = pkg.getParsing ();
	final Object element = read (in, pkg);
	parsing.skipBlanks (in);
	if (in.tryChar (parsing.getTheMarker ()))
	{
	    final LispList the = new LispList ();
	    the.add (theSymbol);
	    the.add (element);
	    readInsideList (in, pkg, the);
	    result.add (the);
	}
	else
	{
	    result.add (element);
	}
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

    /**
     * Read a number or symbol. In most cases this returns an Object. However, in the case of a
     * method reference (like java.lang.System.out.println) it will return a list: (dot object
     * method) that represents a method call on following arguments. The compiler and interpreter
     * need to recognize this for specially.
     */
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
	final Symbol symbol = readSymbol (pkg, s);
	if (symbol != null)
	{
	    return symbol;
	}
	return readJavaSymbol (s);
    }

    public Symbol readSymbol (final Package pkg, final String name)
    {
	// java.util.logging.Level.SEVERE
	final int pos = name.indexOf (Symbol.PACKAGE_SEPARATOR);
	if (pos >= 0)
	{
	    // Process package prefix
	    final String packageName = name.substring (0, pos);
	    final Package p = PackageFactory.findPackage (packageName);
	    if (p != null)
	    {
		final String symbolName = name.substring (pos + 1);
		final Symbol result = p.internSymbol (symbolName);
		return result;
	    }
	    return null;
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

    private Object readJavaSymbol (final String name)
    {
	// (java.lang.System.currentTimeMillis)
	final java.lang.Package pkg = findJavaPackage (name);
	if (pkg != null)
	{
	    final String packageName = pkg.getName ();
	    final String tail = name.substring (packageName.length ());
	    if (tail.isEmpty ())
	    {
		return pkg;
	    }
	    else
	    {
		try
		{
		    final String[] words = tail.split ("\\.");
		    final String className = words[1];
		    final Class<?> cls = Class.forName (packageName + "." + className);
		    if (words.length == 2)
		    {
			return cls;
		    }
		    if (words.length == 3)
		    {
			// Look for a field or method called memberName
			final String memberName = words[2];
			try
			{
			    final Field field = cls.getField (memberName);
			    final Object object = field.get (cls);
			    return object;
			}
			catch (final IllegalArgumentException | IllegalAccessException | NoSuchFieldException e)
			{
			}
			final Method method = getAnyMethodNamed (cls, memberName);
			if (Modifier.isStatic (method.getModifiers ()))
			{
			    return new LispList (dotSymbol, cls, method);
			}
			else
			{
			    return method;
			}
		    }
		    return followFieldChain (cls, words, 2);
		}
		catch (final ClassNotFoundException e)
		{
		    throw new Error ("Error reading symbol (" + name + ") " + e);
		}
		catch (final SecurityException e)
		{
		    e.printStackTrace ();
		}
	    }
	}
	throw new Error ("Could not read " + name + " as a Java class or method reference");
    }

    private Object followFieldChain (final Class<?> cls, final String[] words, final int p)
    {
	// (java.lang.System.out.printf "Foo %s" 44)
	Object object = cls;
	Class<?> c = cls;
	int pos = p;
	try
	{
	    while (pos < words.length)
	    {
		final Field field = c.getField (words[pos]);
		object = field.get (object);
		c = object.getClass ();
		pos++;
	    }
	}
	catch (final NoSuchFieldException | IllegalArgumentException | IllegalAccessException e)
	{
	}
	if (pos < words.length)
	{
	    final Method method = getAnyMethodNamed (c, words[pos]);
	    if (method == null)
	    {
		throw new Error ("Could not access method from " + c + " . " + words[pos]);
	    }
	    return new LispList (dotSymbol, object, method);
	}
	if (pos < words.length)
	{
	    throw new Error ("Could not access field " + object + " . " + words[pos]);
	}
	return object;
    }

    private Method getAnyMethodNamed (final Class<?> cls, final String methodName)
    {
	for (final Method method : cls.getMethods ())
	{
	    if (method.getName ().equals (methodName))
	    {
		return method;
	    }
	}
	return null;
    }

    private java.lang.Package findJavaPackage (final String name)
    {
	java.lang.Package result = null;
	final String[] parts = name.split ("\\.");
	final StringBuilder buffer = new StringBuilder ();
	for (int i = 0; i < parts.length; i++)
	{
	    buffer.append (parts[i]);
	    final java.lang.Package pp = java.lang.Package.getPackage (buffer.toString ());
	    if (pp != null)
	    {
		result = pp;
	    }
	    buffer.append ('.');
	}

	return result;
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
	    // TODO Slashify
	    buffer.append (element);
	    buffer.append ('"');
	}
	else
	{
	    buffer.append (element);
	}
    }
}
