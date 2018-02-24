
package lisp;

import java.util.*;

public class Package implements Lisp
{
    private Package parent;

    private final List<Package> children = new ArrayList<Package> ();

    private String packageName;

    private final List<Symbol> symbols = new ArrayList<Symbol> ();

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
	for (final Symbol symbol : symbols)
	{
	    if (symbol.getName ().equals (name))
	    {
		return symbol;
	    }
	}
	if (parent != null)
	{
	    return parent.find (name);
	}
	return null;
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
