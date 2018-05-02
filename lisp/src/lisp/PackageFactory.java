
package lisp;

import java.util.*;

public class PackageFactory
{
    private static final String GLOBAL_PACKAGE_NAME = "global";
    private static final String DEFAULT_PACKAGE_NAME = "user";
    private static final String SYSTEM_PACKAGE_NAME = "system";

    private static final String[][] PACKAGE_TREE =
	{
	 {null, GLOBAL_PACKAGE_NAME},
	 {GLOBAL_PACKAGE_NAME, SYSTEM_PACKAGE_NAME},
	 {SYSTEM_PACKAGE_NAME, DEFAULT_PACKAGE_NAME}};

    private static final String[][] CONSTANT_SYMBOLS =
	{
	 {GLOBAL_PACKAGE_NAME, "true", "false"}};

    private static final Map<String, Package> packages = new HashMap<String, Package> ();

    private static Package defaultPackage = null;

    private static boolean initializedp = false;

    static
    {
	init ();
    }

    private static void init ()
    {
	if (!initializedp)
	{
	    initializedp = true;
	    for (final String[] p : PACKAGE_TREE)
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
	    defaultPackage = getPackage (DEFAULT_PACKAGE_NAME);
	    for (final String[] constantDefinition : CONSTANT_SYMBOLS)
	    {
		final Package global = getPackage (constantDefinition[0]);
		for (int i = 1; i < constantDefinition.length; i++)
		{
		    final Symbol symbol = global.intern (constantDefinition[i]);
		    symbol.setValue (symbol);
		    symbol.setConstantValue (true);
		}
	    }
	}
    }

    public static Package getSystemPackage ()
    {
	init ();
	return getPackage (SYSTEM_PACKAGE_NAME);
    }

    public static Package getDefaultPackage ()
    {
	return defaultPackage;
    }

    public static void setDefaultPackage (final Package pkg)
    {
	defaultPackage = pkg;
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
