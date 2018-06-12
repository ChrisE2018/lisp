
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

    private static final char BACKSLASH = '\\';
    private static final char DOUBLEQUOTE = '"';

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

    /**
     * Combined list of Lisp Package, Lisp Symbol, Java package and Java class all of which are made
     * easily available for typing.
     */
    private final List<Object> imports = new ArrayList<Object> ();
    // private final List<Package> importedPackages = new ArrayList<Package> ();
    // FIXME Generalize the importedPackages to a new interface for Resolver.
    // Give each resolver a chance to handle an an atom and return a result.
    // First resolver to produce a result wins.
    // Resolvers can correspond with Lisp Package or Java Package or with a map from String to
    // imported Objects.

    private Package currentPackage;

    private final Symbol theSymbol;
    private final Symbol dotSymbol;

    public LispReader ()
    {
	addImport (PackageFactory.getSystemPackage ());
	currentPackage = PackageFactory.getDefaultPackage ();
	theSymbol = PackageFactory.getSystemPackage ().internSymbol ("the");
	dotSymbol = PackageFactory.getSystemPackage ().internSymbol ("dot");
    }

    /** Import a Lisp package. */
    public void addImport (final Package pkg)
    {
	if (!imports.contains (pkg))
	{
	    imports.add (pkg);
	}
    }

    /** Import a Lisp symbol. */
    public void addImport (final Symbol symbol)
    {
	if (!imports.contains (symbol))
	{
	    imports.add (symbol);
	}
    }

    /** Import a Lisp symbol. */
    public void addImport (final java.lang.Package pkg)
    {
	if (!imports.contains (pkg))
	{
	    imports.add (pkg);
	}
    }

    /** Import a Java class. */
    public void addImport (final Class<?> claz)
    {
	if (!imports.contains (claz))
	{
	    imports.add (claz);
	}
    }

    /** Remove a Lisp package import. */
    public void removeImport (final Package pkg)
    {
	imports.remove (pkg);
    }

    /** Remove a Lisp symbol import. */
    public void removeImport (final Symbol symbol)
    {
	imports.remove (symbol);
    }

    /** Remove a Lisp symbol import. */
    public void removeImport (final java.lang.Package pkg)
    {
	imports.remove (pkg);
    }

    /** Remove a Java class import. */
    public void removeImport (final Class<?> claz)
    {
	imports.remove (claz);
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
	if (chr == DOUBLEQUOTE)
	{
	    return readString (DOUBLEQUOTE, in);
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
	    // Handling dots is difficult. As currently implemented dots inside atoms get processed
	    // below and turn into complex read-time references to Java packages, classes and
	    // fields. The field values are resolved at read time, which is not correct but works
	    // for System.out, mostly.
	    // Consider:
	    // (java.lang.System.printf "Foo %s" 47)
	    // If the dot is made not to be an atom char, then dot expressions come out like:
	    // (java dot lang dot System dot printf "Foo %s" 47)
	    // This could be resolved by the interpreter (but currently is not). However, if the
	    // reader imports java.lang there is a problem. Returning (System dot printf "Foo %s"
	    // 47) won't work because the interpreter does not know what packages the reader
	    // imports, so the reader really has to resolve the reference.
	    // Another problem is what to do about spaces inside dotted notation:
	    // (java.lang.System .printf "Foo %s" 47)
	    // Currently java.lang.System will resolve to the class, but .printf will fail.
	    // Also consider when the atom is at the top level:
	    // java.lang.System.printf - can't be read as a list.
	    // java.lang.System .printf - read returns after "System" unless we know we are reading
	    // to the end of a line.
	    in.read (chr); // Discard dot character
	    // Nothing fancy, let interpreter/compiler figure it out.
	    return dotSymbol;
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
	char input = in.read ();
	final StringBuilder buffer = new StringBuilder ();
	// Need to handle embedded slashes
	while (input != stringDelimiter)
	{
	    if (input == BACKSLASH)
	    {
		input = in.read ();
	    }
	    buffer.append (input);
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
	final String name = buffer.toString ();
	if (name.length () == 0)
	{
	    throw new IOException ("Character " + (in.peek ()) + " cannot start an atom");
	}
	// Try parsing an integer
	try
	{
	    // [MAYBE] Create a table for small integers to save memory.
	    final int value = Integer.parseInt (name);
	    return new Integer (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Try parsing a double
	try
	{
	    final double value = Double.parseDouble (name);
	    return new Double (value);
	}
	catch (final NumberFormatException e)
	{

	}
	// Not a number. Return a symbol or Java reference.
	if (name.indexOf (Symbol.PACKAGE_SEPARATOR) >= 0)
	{
	    final Object result = readQualifiedAtom (name);
	    if (result != null)
	    {
		return result;
	    }
	}

	// Use imports and name to find reference
	for (final Object x : imports)
	{
	    if (x instanceof Package)
	    {
		final Package p = (Package)x;
		final Symbol result = p.findSymbol (name);
		if (result != null)
		{
		    return result;
		}
	    }
	    else if (x instanceof Symbol)
	    {
		final Symbol symbol = (Symbol)x;
		if (symbol.is (name))
		{
		    return symbol;
		}
	    }
	    else if (x instanceof java.lang.Package)
	    {
		final java.lang.Package jpkg = (java.lang.Package)x;
		final String packageName = jpkg.getName ();
		final Object result = readQualifiedAtom (packageName + "." + name);
		if (result != null)
		{
		    return result;
		}
		// try
		// {
		// final java.lang.Package jpkg = (java.lang.Package)x;
		// final String packageName = jpkg.getName ();
		// final Class<?> cls = Class.forName (packageName + "." + name);
		// if (cls != null)
		// {
		// return cls;
		// }
		// }
		// catch (final ClassNotFoundException e)
		// {
		// }
	    }
	    else if (x instanceof Class)
	    {
		// Look for a field or method called memberName
		final Class<?> cls = (Class<?>)x;
		if (cls.getSimpleName ().equals (name))
		{
		    return cls;
		}
	    }
	}
	// Intern the name in the reader default package
	return pkg.internSymbol (name);

    }

    private Object readQualifiedAtom (final String name)
    {
	final int pos = name.indexOf (Symbol.PACKAGE_SEPARATOR);
	final String packageName = name.substring (0, pos);
	final Package lpkg = PackageFactory.findPackage (packageName);
	if (lpkg != null)
	{
	    final String symbolName = name.substring (pos + 1);
	    return lpkg.internSymbol (symbolName);
	}
	final java.lang.Package jpkg = findJavaPackage (name);
	if (jpkg != null)
	{
	    final String jpackageName = jpkg.getName ();
	    final String tail = name.substring (jpackageName.length ());
	    if (tail.isEmpty ())
	    {
		return jpkg;
	    }
	    // (java.lang.System.currentTimeMillis)
	    return readJavaObject (jpkg, tail);
	}
	return null;
    }

    private Object readJavaObject (final java.lang.Package pkg, final String tail)
    {
	try
	{
	    final String[] words = tail.split ("\\.");
	    final String className = words[1];
	    final String packageName = pkg.getName ();
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
	    throw new Error ("Error reading symbol (" + tail + ") " + e);
	}
	catch (final SecurityException e)
	{
	    e.printStackTrace ();
	}
	throw new Error ("Could not read " + tail + " as a Java class or method reference");
    }

    private Package findLispPackage (final String name)
    {
	final int pos = name.indexOf (Symbol.PACKAGE_SEPARATOR);
	if (pos >= 0)
	{
	    // Process package prefix
	    final String packageName = name.substring (0, pos);
	    final Package p = PackageFactory.findPackage (packageName);
	    return p;
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

    /**
     * Read a symbol using the specified default package.
     *
     * @param pkg The package to intern the symbol into, if not specified explicitly.
     * @param name The print representation of the symbol. If this contains a package specifier,
     *            then the package argument is ignored.
     * @return An interned symbol, or null if it could not be read. The only case it can't be read
     *         is when the explicit package specifier is not a valid Lisp package.
     */
    public Symbol readSymbol (final Package pkg, final String name)
    {
	// java.util.logging.Level.SEVERE
	final Package p = findLispPackage (name);
	if (p != null)
	{
	    final int pos = name.indexOf (Symbol.PACKAGE_SEPARATOR);
	    final String symbolName = name.substring (pos + 1);
	    return p.internSymbol (symbolName);
	}
	// CONSIDER Eliminate above here and let interpreter/compiler handle dot forms
	final Symbol result = findImportedSymbol (name);
	if (result != null)
	{
	    return result;
	}
	return pkg.internSymbol (name);
    }

    /**
     * Lookup a name to determine if it represents an imported symbol. This is used by the Symbol
     * class to determine how it should print in such a way that the current reader would read it
     * back correctly.
     */
    Symbol findImportedSymbol (final String name)
    {
	for (final Object x : imports)
	{
	    if (x instanceof Package)
	    {
		final Package p = (Package)x;
		final Symbol result = p.findSymbol (name);
		if (result != null)
		{
		    return result;
		}
	    }
	    else if (x instanceof Symbol)
	    {
		final Symbol symbol = (Symbol)x;
		if (symbol.is (name))
		{
		    return symbol;
		}
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
	    final String str = (String)element;
	    buffer.append (DOUBLEQUOTE);
	    for (int i = 0; i < str.length (); i++)
	    {
		// Slashify
		final char c = str.charAt (i);
		if (c == DOUBLEQUOTE || c == BACKSLASH)
		{
		    buffer.append (BACKSLASH);
		}
		buffer.append (c);
	    }
	    buffer.append (DOUBLEQUOTE);
	}
	else
	{
	    buffer.append (element);
	}
    }
}
