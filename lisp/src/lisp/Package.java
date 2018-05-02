
package lisp;

import java.util.*;
import java.util.Map.Entry;

/**
 * Packages contain symbols and help the reader function. <br>
 * [TODO] Define usePackage mechanism to extend parent child hierarchy. <br>
 * [TODO] Consider internal/external symbols.
 *
 * @author cre
 */
public class Package implements Described
{
    /** Control over parsing syntax is collected into the Parsing object. */
    // private static final Parsing DEFAULT_PARSING = new Parsing ();

    private static final String PARSING_VARIABLE = "*parser*";

    private static int PACKAGE_ID = 0;

    private final int packageId = ++PACKAGE_ID;

    /** Parent package to use for looking up symbols not found locally. */
    private final Package parent;

    /** Name of this package. */
    private final String packageName;

    /** Symbols interned into this package locally. */
    private final Map<String, Symbol> symbols = new HashMap<String, Symbol> ();

    public Package (final Package parent, final String name)
    {
	this.parent = parent;
	packageName = name;
	// [TODO] Use a logger
	System.out.printf ("Creating Package %s\n", this);
    }

    /** Parent package to use for looking up symbols not found locally. */
    public Package getParent ()
    {
	return parent;
    }

    /** Child packages that inherit symbols from this package. */
    public LispList getChildren ()
    {
	final LispList children = new LispList ();
	for (final Package pkg : PackageFactory.getPackageMap ().values ())
	{
	    if (pkg.getParent () == this)
	    {
		children.add (pkg);
	    }
	}
	return children;
    }

    /** Name of this package. */
    public String getName ()
    {
	return packageName;
    }

    /**
     * Lookup a symbol locally or by inheritance but do not create the symbol if it is not already
     * present.
     */
    public Symbol find (final String name)
    {
	final Symbol result = symbols.get (name);
	if (result != null)
	{
	    return result;
	}
	if (parent != null)
	{
	    return parent.find (name);
	}
	return null;
    }

    /**
     * Lookup a symbol locally and create it if not present. Does not inherit from parent packages.
     */
    public Symbol internLocal (final String name)
    {
	Symbol result = symbols.get (name);
	if (result != null)
	{
	    return result;
	}
	result = new Symbol (this, name);
	symbols.put (name, result);
	return result;
    }

    /**
     * Lookup a symbol from this package or parent packages (by inheritance). If not found, create
     * it locally.
     */
    public Symbol intern (final String name)
    {
	Symbol result = find (name);
	if (result != null)
	{
	    return result;
	}
	if (name.equals ("quote"))
	{
	    System.out.printf ("Quote interned %s\n", this);
	}
	result = new Symbol (this, name);
	symbols.put (name, result);
	return result;
    }

    /** Control over parsing syntax is collected into a Parsing object. */
    public Parsing getParsing ()
    {
	final Symbol symbol = find (PARSING_VARIABLE);
	if (symbol != null)
	{
	    final Object value = symbol.getValue ();
	    if (value instanceof Parsing)
	    {
		return (Parsing)value;
	    }
	}
	return Parsing.getDefaultParsing ();
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (toString ());
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (packageName);
	buffer.append (" ");
	buffer.append (packageId);
	if (parent != null)
	{
	    buffer.append (" parent: ");
	    buffer.append (parent);
	}
	buffer.append (">");
	return buffer.toString ();
    }

    @Override
    public void describe ()
    {
	System.out.printf ("Parent: %s \n", parent);
	for (final Entry<String, Symbol> entry : symbols.entrySet ())
	{
	    System.out.printf ("   Entry %s: %s \n", entry.getKey (), entry.getValue ());
	}
    }
}
