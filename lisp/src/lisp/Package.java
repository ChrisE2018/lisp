
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
public class Package implements Describer
{
    private static final String PARSING_VARIABLE = "*parser*";

    private static int PACKAGE_ID = 0;

    private final int packageId = ++PACKAGE_ID;

    /** Parent package to use for looking up symbols not found locally. */
    private final Package parent;

    /** Name of this package. */
    private final String packageName;

    /** Private symbols interned into this package locally. */
    private final Map<String, Symbol> privateSymbols = new HashMap<String, Symbol> ();

    /** Public symbols interned into this package locally. */
    private final Map<String, Symbol> publicSymbols = new HashMap<String, Symbol> ();

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
     * Lookup a public symbol locally or by inheritance but do not create the symbol if it is not
     * already present.
     */
    public Symbol findPublic (final String name)
    {
	Symbol result = publicSymbols.get (name);
	if (result == null)
	{
	    if (parent != null)
	    {
		result = parent.findPublic (name);
	    }
	}
	return result;
    }

    /**
     * Lookup a public or private symbol locally or by inheritance but do not create the symbol if
     * it is not already present.
     */
    public Symbol findPrivate (final String name)
    {
	Symbol result = publicSymbols.get (name);
	if (result == null)
	{
	    result = privateSymbols.get (name);
	    if (result == null)
	    {
		if (parent != null)
		{
		    result = parent.findPrivate (name);
		}
	    }
	}
	return result;
    }

    /**
     * Lookup a symbol from this package or parent packages (by inheritance). If not found, create
     * it locally.
     */
    public Symbol internPrivate (final String name)
    {
	Symbol result = findPrivate (name);
	if (result == null)
	{
	    result = new Symbol (this, name);
	    privateSymbols.put (name, result);
	}
	return result;
    }

    /**
     * Lookup a public symbol from this package or parent packages (by inheritance). If not found,
     * create it locally and make it public.
     */
    public Symbol internPublic (final String name)
    {
	Symbol result = findPublic (name);
	if (result == null)
	{
	    result = privateSymbols.get (name);
	    if (result != null)
	    {
		privateSymbols.remove (name);
	    }
	    else
	    {
		result = new Symbol (this, name);
	    }
	    publicSymbols.put (name, result);
	}
	return result;
    }

    public Object isPublic (final Symbol symbol)
    {
	return publicSymbols.values ().contains (symbol);
    }

    /** Control over parsing syntax is collected into a Parsing object. */
    public Parsing getParsing ()
    {
	final Symbol symbol = findPublic (PARSING_VARIABLE);
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
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Id", packageId);
	result.put ("Parent", parent);
	int childCount = 0;
	for (final Object child : getChildren ())
	{
	    result.put ("Child" + childCount, child);
	    childCount++;
	}
	result.put ("Public", publicSymbols.size ());
	getSymbolDescriptions (result, publicSymbols, 5);
	result.put ("Private", privateSymbols.size ());
	getSymbolDescriptions (result, privateSymbols, 5);
	return result;
    }

    private void getSymbolDescriptions (final Map<String, Object> result, final Map<String, Symbol> symbols, final int limit)
    {
	int count = 0;
	for (final Entry<String, Symbol> entry : symbols.entrySet ())
	{
	    if (++count > limit)
	    {
		return;
	    }
	    result.put (entry.getKey (), entry.getValue ());
	}
    }
}
