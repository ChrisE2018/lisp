
package lisp;

import java.util.*;

public class PackageFactory
{
    private static final String[][] INITIAL_PACKAGES =
	{
	 {null, "global"},
	 {"global", "system"},
	 {"system", "user"}};

    private static final Map<String, Package> packages = new HashMap<String, Package> ();

    private static boolean initializedp = false;

    private static void init ()
    {
	if (!initializedp)
	{
	    initializedp = true;
	    for (final String[] p : INITIAL_PACKAGES)
	    {
		final String parentName = p[0];
		final String childName = p[1];
		if (parentName == null)
		{
		    getPackage (childName);
		}
		else
		{
		    final Package parent = getPackage (parentName);
		    final Package child = new Package (parent, childName);
		    packages.put (childName, child);
		}
	    }
	    final Package global = getPackage ("global");
	    final Symbol trueSymbol = global.intern ("true");
	    final Symbol falseSymbol = global.intern ("false");
	    trueSymbol.setValue (trueSymbol);
	    falseSymbol.setValue (falseSymbol);
	    trueSymbol.setConstantValue (true);
	    falseSymbol.setConstantValue (true);
	}
    }

    public static Package getSystemPackage ()
    {
	init ();
	return getPackage ("system");
    }

    public static Package getPackage (final String name)
    {
	Package result = packages.get (name);
	if (result == null)
	{
	    result = new Package (null, name);
	    packages.put (name, result);
	}
	return result;
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
}
