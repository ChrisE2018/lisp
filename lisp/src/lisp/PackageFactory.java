
package lisp;

import java.util.*;

public class PackageFactory
{
    private static final Map<String, Package> packages = new HashMap<String, Package> ();

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
