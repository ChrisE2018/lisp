
package lisp.primitives;

import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.symbol.*;

/** Primitives for packages and symbols. */
public class PackagePrimitives extends Definer
{
    @DefineLisp
    public Object inPackage (final Object pkg)
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	final Package p = coercePackage (pkg, true);
	lispReader.setCurrentPackage (p);
	return pkg;
    }

    @DefineLisp
    public LispList getImports ()
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	return lispReader.getImports ();
    }

    /**
     * Add an import to a Package, Symbol, Java Package or Java Class. This is kind of ugly and
     * perhaps could be condensed.
     *
     * @param reference Package, Symbol, Java Package, Java Class or String.
     * @return The object added to the imports list.
     */
    @DefineLisp (name = "import")
    public Object addImport (final Object reference)
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	final Object object = getImportsObject (reference);
	if (object instanceof Package)
	{
	    lispReader.addImport ((Package)object);
	}
	else if (object instanceof Symbol)
	{
	    lispReader.addImport ((Symbol)object);
	}
	else if (object instanceof java.lang.Package)
	{
	    lispReader.addImport ((java.lang.Package)object);
	}
	else if (object instanceof Class)
	{
	    lispReader.addImport ((Class<?>)object);
	}
	return object;
    }

    @DefineLisp
    public Object removeImport (final Object reference)
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	final Object object = getImportsObject (reference);
	if (object instanceof Package)
	{
	    lispReader.removeImport ((Package)object);
	}
	else if (object instanceof Symbol)
	{
	    lispReader.removeImport ((Symbol)object);
	}
	else if (object instanceof java.lang.Package)
	{
	    lispReader.removeImport ((java.lang.Package)object);
	}
	else if (object instanceof Class)
	{
	    lispReader.removeImport ((Class<?>)object);
	}
	return object;
    }

    private Object getImportsObject (final Object reference)
    {
	if ((reference instanceof Package) || (reference instanceof Symbol) || (reference instanceof java.lang.Package)
	    || (reference instanceof Class))
	{
	    return reference;
	}
	else if (reference instanceof String)
	{
	    final String name = (String)reference;
	    final Package lpkg = PackageFactory.findPackage (name);
	    if (lpkg != null)
	    {
		return lpkg;
	    }
	    try
	    {
		final Class<?> cls = Class.forName (name);
		if (cls != null)
		{
		    return cls;
		}
	    }
	    catch (final ClassNotFoundException e)
	    {
	    }
	    final java.lang.Package jpkg = java.lang.Package.getPackage (name);
	    if (jpkg != null)
	    {
		return jpkg;
	    }
	}
	return null;
    }

    /**
     * Lookup and return the default (user) package.
     *
     * @param arguments
     */
    @DefineLisp
    public Object getDefaultPackage ()
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	return lispReader.getCurrentPackage ();
    }

    /**
     * Lookup and return the system package.
     *
     * @param arguments
     */
    @DefineLisp
    public Object getSystemPackage ()
    {
	return PackageFactory.getSystemPackage ();
    }

    // Symbols

    @DefineLisp
    public Object symbolFunction (final Symbol arg)
    {
	return arg.getFunction ();
    }

    @DefineLisp
    public Object symbolPlist (final Symbol arg)
    {
	return arg.getPlist ();
    }

    @DefineLisp
    public Object get (final Symbol arg, final Symbol key)
    {
	return arg.get (key);
    }

    @DefineLisp
    public Object put (final Symbol arg, final Symbol key, final Object value)
    {
	return arg.put (key, value);
    }

    @DefineLisp
    public Object remProp (final Symbol arg, final Symbol key)
    {
	return arg.remove (key);
    }

    @DefineLisp
    public Object getAllSymbols ()
    {
	final LispList result = new LispList ();
	for (final Package pkg : PackageFactory.getPackageMap ().values ())
	{
	    for (final Symbol symbol : pkg.getPackageSymbols ())
	    {
		result.add (symbol);
	    }
	}
	return result;
    }

    @DefineLisp
    public Object getAllSymbols (final Package pkg)
    {
	final LispList result = new LispList ();
	for (final Symbol symbol : pkg.getPackageSymbols ())
	{
	    result.add (symbol);
	}
	return result;
    }

    @DefineLisp
    public Object getAllFunctionSymbols ()
    {
	final LispList result = new LispList ();
	for (final Package pkg : PackageFactory.getPackageMap ().values ())
	{
	    for (final Symbol symbol : pkg.getPackageSymbols ())
	    {
		if (symbol.getFunction () != null)
		{
		    result.add (symbol);
		}
	    }
	}
	return result;
    }

    @DefineLisp
    public Object getAllSpecialFunctionSymbols ()
    {
	final LispList result = new LispList ();
	for (final Package pkg : PackageFactory.getPackageMap ().values ())
	{
	    for (final Symbol symbol : pkg.getPackageSymbols ())
	    {
		final FunctionCell fc = symbol.getFunction ();
		if (fc != null && fc instanceof SpecialFunctionCell)
		{
		    result.add (symbol);
		}
	    }
	}
	return result;
    }

    @DefineLisp
    public LispReader getLispReader ()
    {
	return LispReader.getLispThreadReader ();
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
