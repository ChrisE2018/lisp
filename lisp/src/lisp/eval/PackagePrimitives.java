
package lisp.eval;

import lisp.*;
import lisp.Package;
import lisp.symbol.*;

/** Primitives for packages and symbols. */
public class PackagePrimitives extends Definer
{
    @DefineLisp
    public Object inPackage (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	final LispReader lispReader = LispReader.getLispThreadReader ();
	lispReader.setCurrentPackage (p);
	return pkg;
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

    // @DefineLisp
    // public Object getParentPackages (final Object pkg)
    // {
    // final Package p = coercePackage (pkg, true);
    // final LispList result = new LispList ();
    // result.addAll (p.getParents ());
    // return result;
    // }

    // @DefineLisp
    // public Object getChildPackages (final Object pkg)
    // {
    // final Package p = coercePackage (pkg, true);
    // final LispList result = new LispList ();
    // result.addAll (p.getChildren ());
    // return result;
    // }

    // Symbols

    /**
     * Interpreter for setq statements.
     *
     * @param interpreter The interpreter used to evaluate forms.
     * @param arguments The symbol and value form.
     * @return The new value.
     */
    @DefineLisp (special = true)
    public Object setq (final LexicalContext context, final Symbol symbol, final Object form) throws Exception
    {
	final Object value = context.eval (form);
	symbol.setValue (value);
	return value;
    }

    @DefineLisp
    public Object symbolValue (final Symbol arg)
    {
	return arg.getValue ();
    }

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
	    // for (final Symbol symbol : pkg.getPrivateSymbols ())
	    // {
	    // result.add (symbol);
	    // }
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
	// for (final Symbol symbol : pkg.getPrivateSymbols ())
	// {
	// result.add (symbol);
	// }
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
	    // for (final Symbol symbol : pkg.getPrivateSymbols ())
	    // {
	    // if (symbol.getFunction () != null)
	    // {
	    // result.add (symbol);
	    // }
	    // }
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
	    // for (final Symbol symbol : pkg.getPrivateSymbols ())
	    // {
	    // final FunctionCell fc = symbol.getFunction ();
	    // if (fc != null && fc instanceof SpecialFunctionCell)
	    // {
	    // result.add (symbol);
	    // }
	    // }
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
