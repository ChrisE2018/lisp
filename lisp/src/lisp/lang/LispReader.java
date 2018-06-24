
package lisp.lang;

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
    private static final char COMMA = ',';
    private static final char DOT = '.';

    /**
     * A LispReader is associated with each thread. Return the current one. If none is associated
     * with this thread, return the defaultLispReader instead.
     */
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

    /**
     * A LispReader is associated with each thread. Execute a supplier with the supplied reader made
     * current for this thread.
     */
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

    /** The current package for this reader. */
    private Package currentPackage;

    /**
     * Combined list of Lisp Package, Lisp Symbol, Java package and Java class all of which are made
     * easily available for typing.
     */
    private final LispList imports = new LispList ();

    /**
     * Symbol for 'the'. Initialize this in the constructor so the PackageFactory is already setup.
     */
    private final Symbol theSymbol;

    /**
     * Symbol for 'dot'. Initialize this in the constructor so the PackageFactory is already setup.
     */
    private final Symbol dotSymbol;

    /**
     * Symbol for 'field'. Initialize this in the constructor so the PackageFactory is already
     * setup.
     */
    private final Symbol fieldSymbol;

    /**
     * Default constructor for a LispReader. This will set the current package to the PackageFactory
     * default package and import the system package (lisp.lang) automatically.
     */
    public LispReader ()
    {
	// TODO Define another constructor which does no imports
	currentPackage = PackageFactory.getDefaultPackage ();
	final Package systemPackage = PackageFactory.getSystemPackage ();
	theSymbol = systemPackage.internSymbol ("the");
	dotSymbol = systemPackage.internSymbol ("dot");
	fieldSymbol = systemPackage.internSymbol ("field");
	addImport (systemPackage);
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

    /**
     * Return the list of imported objects. This is a mixed list of Lisp packages, Java packages
     * Lisp Symbols, and Java classes.
     *
     * @return The list of items. Do not modify the list.
     */
    public LispList getImports ()
    {
	return imports;
    }

    /**
     * Return the current package used to intern Symbols read without a package qualifier.
     *
     * @return The current Lisp package for reading Symbols.
     */
    public Package getCurrentPackage ()
    {
	return currentPackage;
    }

    /**
     * Set the current package used to intern Symbols read without a package qualifier.
     *
     * @param currentPackage The new current Lisp package for reading Symbols.
     * @throws NullPointerException If null is given.
     */
    public void setCurrentPackage (final Package currentPackage)
    {
	if (currentPackage == null)
	{
	    throw new NullPointerException ("Can't set the current package to null");
	}
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
	final LispList listResult = parsing.getParenList (chr);
	if (listResult != null)
	{
	    in.read ();
	    in.startLevel ();
	    final Object result = readList (in, pkg, listResult);
	    in.finishLevel ();
	    return result;
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
	    if (in.tryChar (COMMA))
	    {
		parsing.skipBlanks (in);
	    }
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
	// 0) If <name> matches a fully qualified lisp Symbol return that
	// 1) If <name> matches an imported lisp.Symbol return that
	// 2) If <name> matches an existing lisp.Symbol in the current package return that
	// 2.5) If <name> matches an existing lisp package return that
	// 3) If <name> matches an existing lisp.Symbol in an imported package return that
	// Ignore dots in the name: if a symbol somehow has then return it
	// 4) If <name> matches an existing java package return that
	// 4.5) If the prefix matches a lisp package, intern the symbol there.
	// 5) If <name> matches a fully qualified java Class return that
	// 6) If <name> matches an imported java Class return that
	// 7) If <name> matches a java Class in an imported package return that
	// 5') If a prefix matches a fully qualified java Class use it and look for a field or
	// method suffix
	// 6') If a prefix matches an imported java Class use it and look for a field or method
	// suffix
	// 7') If a prefix matches a java Class in an imported package use it and look for a field
	// or method suffix
	// Results:
	// target ::= <class>
	// target ::= (field <target> <fieldname>)
	// target ::= (dot <target> <methodname>)
	// 99) Create a new symbol in the current package and return it
	// Intern the name in the reader default package
	Object result = findFullyQualifiedSymbol (name);
	if (result != null)
	{
	    // 0) If <name> matches a fully qualified lisp Symbol return that
	    return result;
	}
	result = findSimpleImportedSymbol (name);
	if (result != null)
	{
	    // 1) If <name> matches an imported lisp.Symbol return that
	    return result;
	}
	result = pkg.findSymbol (name);
	if (result != null)
	{
	    // 2) If <name> matches an existing lisp.Symbol in the current package return that
	    return result;
	}
	result = PackageFactory.findPackage (name);
	if (result != null)
	{
	    // 2.5) If <name> matches an existing lisp package return that
	    return result;
	}
	result = findPackageImportedSymbol (name);
	if (result != null)
	{
	    // 3) If <name> matches an existing lisp.Symbol in an imported package return that
	    // Ignore dots in the name: if a symbol somehow has then return it
	    return result;
	}
	result = java.lang.Package.getPackage (name);
	if (result != null)
	{
	    // Java Package (rule 4).
	    return result;
	}
	result = checkFullyQualifiedJavaClass (name);
	if (result != null)
	{
	    // Fully qualified Java class (rule 5).
	    return result;
	}
	result = checkImportedJavaClass (name);
	if (result != null)
	{
	    // Imported java Class (rule 6).
	    return result;
	}
	result = checkImportedPackageJavaClass (name);
	if (result != null)
	{
	    // Java Class in an imported package (rule 7).
	    return result;
	}
	final int pos = name.lastIndexOf (DOT);
	if (pos >= 0)
	{
	    final String packageName = name.substring (0, pos);
	    final String symbolName = name.substring (pos + 1);
	    final Package pack = PackageFactory.findPackage (packageName);
	    if (pack != null)
	    {
		result = pack.findSymbol (symbolName);
		if (result != null)
		{
		    return result;
		}
		return pack.internSymbol (symbolName);
	    }
	    throw new IOException ("There is no package named '" + packageName + "' for symbol '" + symbolName + "'");
	}
	return pkg.internSymbol (name);
    }

    private Symbol findFullyQualifiedSymbol (final String name)
    {
	final int pos = name.lastIndexOf (DOT);
	if (pos > 0)
	{
	    final String packageName = name.substring (0, pos);
	    final String symbolName = name.substring (pos + 1);
	    final Package pkg = PackageFactory.findPackage (packageName);
	    if (pkg != null)
	    {
		return pkg.findSymbol (symbolName);
	    }
	}
	return null;
    }

    /**
     * Find a Symbol that has been directly imported. This does not check for symbols in imported
     * packages.
     */
    private Symbol findSimpleImportedSymbol (final String name)
    {
	for (final Object x : imports)
	{
	    if (x instanceof Symbol)
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

    /** Find a symbol in an imported lisp package. */
    private Symbol findPackageImportedSymbol (final String name)
    {
	for (final Object x : imports)
	{
	    if (x instanceof Package)
	    {
		final Package pkg = (Package)x;
		final Symbol result = pkg.findSymbol (name);
		if (result != null)
		{
		    return result;
		}
	    }
	}
	return null;
    }

    private Object checkFullyQualifiedJavaClass (final String name)
    {
	// Rule 5
	// 5) If a prefix matches a fully qualified java Class use it and look for a field or method
	// suffix
	for (int p = name.indexOf (DOT); p > 0; p = name.indexOf (DOT, p + 1))
	{
	    final String prefix = name.substring (0, p);
	    final Class<?> cls = silentClassForName (prefix);
	    if (cls != null)
	    {
		return processClassSuffix (cls, name.substring (p + 1));
	    }
	}
	final Class<?> result = silentClassForName (name);
	if (result != null)
	{
	    return result;
	}
	return null;
    }

    private Object checkImportedJavaClass (final String name)
    {
	// Rule 6
	// 6) If a prefix matches an imported java Class use it and look for a field or method
	// suffix
	for (int p = name.indexOf (DOT); p > 0; p = name.indexOf (DOT, p + 1))
	{
	    final String prefix = name.substring (0, p);
	    final Class<?> cls = findImportedJavaClass (prefix);
	    if (cls != null)
	    {
		return processClassSuffix (cls, name.substring (p + 1));
	    }
	}
	final Class<?> result = findImportedJavaClass (name);
	if (result != null)
	{
	    return result;
	}
	return null;
    }

    private Class<?> findImportedJavaClass (final String name)
    {
	for (final Object x : imports)
	{
	    if (x instanceof Class)
	    {
		final Class<?> cls = (Class<?>)x;
		if (name.equals (cls.getSimpleName ()))
		{
		    return cls;
		}
	    }
	}
	return null;
    }

    private Object checkImportedPackageJavaClass (final String name)
    {
	// Rule 7
	// 7') If a prefix matches a java Class in an imported package use it and look for a field
	// or method suffix
	for (int p = name.indexOf (DOT); p > 0; p = name.indexOf (DOT, p + 1))
	{
	    final String prefix = name.substring (0, p);
	    final Class<?> cls = findImportedPackageJavaClass (prefix);
	    if (cls != null)
	    {
		return processClassSuffix (cls, name.substring (p + 1));
	    }
	}
	final Class<?> result = findImportedPackageJavaClass (name);
	if (result != null)
	{
	    return result;
	}
	return null;
    }

    private Class<?> findImportedPackageJavaClass (final String name)
    {
	for (final Object x : imports)
	{
	    if (x instanceof java.lang.Package)
	    {
		final java.lang.Package pkg = (java.lang.Package)x;
		final Class<?> cls = silentClassForName (pkg.getName () + DOT + name);
		if (cls != null)
		{
		    return cls;
		}
	    }
	}
	return null;
    }

    /**
     * Return a class for a name without throwing any exceptions.
     *
     * @param name The class name.
     * @return The class object or null if it is not found.
     */
    private Class<?> silentClassForName (final String name)
    {
	try
	{
	    final Class<?> result = Class.forName (name);
	    if (result != null)
	    {
		return result;
	    }
	}
	catch (final ClassNotFoundException e)
	{
	}
	return null;
    }

    private Object processClassSuffix (final Class<?> cls, final String suffix)
    {
	// This will have to process things twice: once look at the actual field data
	// to determine types and another to build the result
	final int p = suffix.indexOf (DOT);
	if (p < 0)
	{
	    // Check for field reference
	    // Check for method reference
	    try
	    {
		@SuppressWarnings ("unused")
		final Field field = cls.getField (suffix);
		return new LispList (fieldSymbol, cls, suffix);
	    }
	    catch (final IllegalArgumentException | NoSuchFieldException e)
	    {
	    }
	    final Method method = getAnyMethodNamed (cls, suffix);
	    if (method == null)
	    {
		throw new IllegalArgumentException ("No " + suffix + " method found in " + cls);
	    }
	    if (Modifier.isStatic (method.getModifiers ()))
	    {
		return new LispList (dotSymbol, cls, suffix);
	    }
	    else
	    {
		return suffix;
	    }
	}
	else
	{
	    // System.out.println ==> ("println" (field java.lang.System "out"))
	    final String memberName = suffix.substring (0, p);
	    final Class<?> referenceClass = determineReferenceClass (cls, memberName);
	    final Object o = processClassSuffix (referenceClass, suffix.substring (p + 1));
	    try
	    {
		@SuppressWarnings ("unused")
		final Field field = cls.getField (memberName);
		return new LispList (dotSymbol, new LispList (fieldSymbol, cls, memberName), o);
	    }
	    catch (final IllegalArgumentException | NoSuchFieldException e)
	    {
	    }
	    final Method method = getAnyMethodNamed (cls, memberName);
	    if (method == null)
	    {
		throw new IllegalArgumentException ("No " + memberName + " method found in " + cls);
	    }
	    if (Modifier.isStatic (method.getModifiers ()))
	    {
		return new LispList (dotSymbol, new LispList (fieldSymbol, cls, memberName), o);
	    }
	    else
	    {
		return new LispList (dotSymbol, cls, suffix);
	    }
	}
    }

    private Class<?> determineReferenceClass (final Class<?> cls, final String suffix)
    {
	// This will have to process things twice: once look at the actual field data
	// to determine types and another to build the result
	final int p = suffix.indexOf (DOT);
	if (p < 0)
	{
	    // Check for field reference
	    // Check for method reference
	    try
	    {
		final Field field = cls.getField (suffix);
		final Object object = field.get (cls);
		return object.getClass ();
	    }
	    catch (final IllegalArgumentException | IllegalAccessException | NoSuchFieldException e)
	    {
	    }
	    final Method method = getAnyMethodNamed (cls, suffix);
	    // if (Modifier.isStatic (method.getModifiers ()))
	    // {
	    // return new LispList (dotSymbol, cls, method);
	    // }
	    // else
	    {
		// Not correct: method overload may not be selected
		return method.getReturnType ();
	    }
	}
	final Class<?> reference = determineReferenceClass (cls, suffix.substring (0, p));
	return determineReferenceClass (reference, suffix.substring (p + 1));
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
     * @param name The print representation of the symbol. If <name> contains a package specifier,
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
