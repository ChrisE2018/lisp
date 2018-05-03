
package lisp;

import java.util.*;

public class PackageFactory
{
    private static final String GLOBAL_PACKAGE_NAME = "global";
    private static final String DEFAULT_PACKAGE_NAME = "user";
    private static final String SYSTEM_PACKAGE_NAME = "system";

    private static boolean initializedp = false;

    private static final String[][] PACKAGE_TREE =
	{
	 {null, GLOBAL_PACKAGE_NAME},
	 {GLOBAL_PACKAGE_NAME, SYSTEM_PACKAGE_NAME},
	 {SYSTEM_PACKAGE_NAME, DEFAULT_PACKAGE_NAME}};

    private static final Object[][] CONSTANT_SYMBOLS =
	{
	 {GLOBAL_PACKAGE_NAME, "true", Boolean.TRUE, "false", Boolean.FALSE}};

    private static final Map<String, Package> packages = new HashMap<String, Package> ();

    private static Package defaultPackage = null;

    private PackageFactory ()
    {
	// no instances
    }

    static
    {
	init ();
    }

    private synchronized static void init ()
    {
	if (!initializedp)
	{
	    // [TODO] Use a logger
	    System.out.printf ("Initializing PackageFactory\n");
	    initializedp = true;
	    for (final String[] p : PACKAGE_TREE)
	    {
		final String parentName = p[0];
		final String childName = p[1];
		if (parentName == null)
		{
		    getPackage (null, childName);
		}
		else
		{
		    final Package child = getPackage (parentName, childName);
		    packages.put (childName, child);
		}
	    }
	    defaultPackage = getPackage (GLOBAL_PACKAGE_NAME, DEFAULT_PACKAGE_NAME);
	    for (final Object[] constantDefinition : CONSTANT_SYMBOLS)
	    {
		final String packageName = (String)constantDefinition[0];
		final Package pkg = getPackage (GLOBAL_PACKAGE_NAME, packageName);
		for (int i = 1; i < constantDefinition.length; i += 2)
		{
		    final String symbolName = (String)constantDefinition[i];
		    final Object value = constantDefinition[i + 1];
		    final Symbol symbol = pkg.internPublic (symbolName);
		    symbol.setValue (value);
		    symbol.setConstantValue (true);
		}
	    }
	}
    }

    public static Package getSystemPackage ()
    {
	init ();
	return getPackage (GLOBAL_PACKAGE_NAME, SYSTEM_PACKAGE_NAME);
    }

    public static Package getDefaultPackage ()
    {
	return defaultPackage;
    }

    public static void setDefaultPackage (final Package pkg)
    {
	defaultPackage = pkg;
    }

    public static Package getPackage (final String parentName, final String packageName)
    {
	Package result = packages.get (packageName);
	if (result == null)
	{
	    final Package parent = packages.get (parentName);
	    result = new Package (parent, packageName);
	    packages.put (packageName, result);
	}
	return result;
    }

    public static Package getPackage (final String packageName)
    {
	final Package result = packages.get (packageName);
	if (result == null)
	{
	    throw new IllegalArgumentException ("There is no package named " + packageName);
	}
	return result;
    }

    public static Map<String, Package> getPackageMap ()
    {
	return packages;
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
