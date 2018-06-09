
package lisp;

import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import lisp.util.MultiMap;

/**
 * Packages contain symbols and help the reader function. Symbols can be internal (private) or
 * external (public). A symbol lookup in the current (default) package will create a private symbol.
 * A normal reference to another package will only see public symbols.
 *
 * @author cre
 */
public class Package implements Describer
{
    private static final Logger LOGGER = Logger.getLogger (Package.class.getName ());
    private static final String PARSING_VARIABLE = "*parser*";

    /** Name of this package. */
    private final String packageName;

    /** Public symbols interned into this package. */
    private final Map<String, Symbol> packageSymbols = new HashMap<String, Symbol> ();

    public Package (final String name)
    {
	packageName = name;
	LOGGER.info ("Creating Package " + name);
    }

    /** Name of this package. */
    public String getName ()
    {
	return packageName;
    }

    /**
     * Lookup a package symbol but do not create the symbol if it is not already present.
     */
    public Symbol findSymbol (final String name)
    {
	final Symbol result = packageSymbols.get (name);
	return result;
    }

    /**
     * Lookup a symbol from this package. If not found, create it.
     */
    public Symbol internSymbol (final String name)
    {
	Symbol result = findSymbol (name);
	if (result == null)
	{
	    result = new Symbol (this, name);
	    packageSymbols.put (name, result);
	}
	return result;
    }

    public Collection<Symbol> getPackageSymbols ()
    {
	return packageSymbols.values ();
    }

    /** Control over parsing syntax is collected into a Parsing object. */
    public Parsing getParsing ()
    {
	final Symbol symbol = findSymbol (PARSING_VARIABLE);
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
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }

    @Override
    public MultiMap<String, Object> getDescriberValues (final Object target)
    {
	final MultiMap<String, Object> result = new MultiMap<String, Object> ();
	getSymbolDescriptions (result, packageSymbols, "Public", 5);
	return result;
    }

    private void getSymbolDescriptions (final MultiMap<String, Object> result, final Map<String, Symbol> symbols,
            final String prefix, final int limit)
    {
	int count = 0;
	for (final Entry<String, Symbol> entry : symbols.entrySet ())
	{
	    if (++count > limit)
	    {
		final int extra = symbols.size () - limit;
		result.put ("Hidden " + prefix + " symbols", extra);
		return;
	    }
	    final String key = prefix + "-" + count + " " + entry.getKey ();
	    result.put (key, entry.getValue ());
	}
    }
}
