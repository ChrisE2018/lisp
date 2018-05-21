
package lisp;

import java.util.*;

public class PackageFactory
{
    private static final String GLOBAL_PACKAGE_NAME = "global";
    private static final String DEFAULT_PACKAGE_NAME = "user";
    private static final String SYSTEM_PACKAGE_NAME = "system";

    private static boolean initializedp = false;

    /** Names of packages to create initially. */
    private static final String[] INITIAL_PACKAGES =
	{GLOBAL_PACKAGE_NAME, SYSTEM_PACKAGE_NAME, DEFAULT_PACKAGE_NAME};

    /**
     * Package use linking. Each entry is the packageName followed by the names of packages it uses.
     */
    private static final String[][] PACKAGE_USES =
	{
	 {SYSTEM_PACKAGE_NAME, GLOBAL_PACKAGE_NAME},
	 {DEFAULT_PACKAGE_NAME, GLOBAL_PACKAGE_NAME, SYSTEM_PACKAGE_NAME}};

    /**
     * Predefined constant values. Each entry is packageName, (symbolName, symbolValue)*
     */
    private static final Object[][] CONSTANT_SYMBOLS =
	{
	 {GLOBAL_PACKAGE_NAME, "true", Boolean.TRUE, "false", Boolean.FALSE, "null", null}};

    /** Map from package name to package object for all packages that exist. */
    private static final Map<String, Package> packages = new HashMap<String, Package> ();

    /** Package we are working in currently. */
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
	    for (final String packageName : INITIAL_PACKAGES)
	    {
		final Package pkg = new Package (packageName);
		packages.put (packageName, pkg);
	    }
	    for (final String[] p : PACKAGE_USES)
	    {
		final String packageName = p[0];
		final Package pkg = getPackage (packageName);
		for (int i = 1; i < p.length; i++)
		{
		    final String usedPackageName = p[i];
		    final Package usedPackage = getPackage (usedPackageName);
		    pkg.usePackage (usedPackage);
		}
	    }
	    defaultPackage = getPackage (DEFAULT_PACKAGE_NAME);
	    for (final Object[] constantDefinition : CONSTANT_SYMBOLS)
	    {
		final String packageName = (String)constantDefinition[0];
		final Package pkg = getPackage (packageName);
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
