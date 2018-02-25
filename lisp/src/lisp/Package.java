
package lisp;

import java.util.*;

/**
 * Packages contain symbols and help the reader function. [TODO] Define usePackage mechanism to
 * extend parent child hierarchy. [TODO] Consider internal/external symbols.
 *
 * @author cre
 */
public class Package implements Lisp
{
    private final Package parent;

    private final List<Package> children = new ArrayList<Package> ();

    private final String packageName;

    private final Map<String, Symbol> symbols = new HashMap<String, Symbol> ();

    public Package (final Package parent, final String name)
    {
	this.parent = parent;
	packageName = name;
    }

    public Package getParent ()
    {
	return parent;
    }

    public List<Package> getChildren ()
    {
	return children;
    }

    public String getName ()
    {
	return packageName;
    }

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

    public Symbol intern (final String name)
    {
	Symbol result = find (name);
	if (result != null)
	{
	    return result;
	}
	result = new Symbol (this, name);
	symbols.put (name, result);
	return result;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append ("#<package ");
	buffer.append (packageName);
	buffer.append (">");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (packageName);
	buffer.append (">");
	return buffer.toString ();
    }
}
