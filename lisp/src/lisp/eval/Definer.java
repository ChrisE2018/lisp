
package lisp.eval;

import java.lang.reflect.Method;
import java.util.List;

import lisp.*;
import lisp.Package;

/** Helper class (used as a base class) for defining functions. */
public class Definer
{
    private final Package pkg;
    private final Object source;

    public Definer (final Package pkg, final Object source)
    {
	this.pkg = pkg;
	this.source = source;
    }

    public Definer (final Package pkg)
    {
	this.pkg = pkg;
	source = this;
    }

    public void define (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new StandardFunctionCell (source, method));
    }

    public void defspecial (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new SpecialFunctionCell (source, method));
    }

    public void defmacro (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new MacroFunctionCell (source, method));
    }

    public String coerceString (final Object arg, final boolean errorp)
    {
	if (arg != null)
	{
	    if (arg instanceof String)
	    {
		return (String)arg;
	    }
	    if (arg instanceof Symbol)
	    {
		return ((Symbol)arg).getName ();
	    }
	}
	if (errorp)
	{
	    throw new IllegalArgumentException ("Can't coerce object to String: " + arg);
	}
	return null;
    }

    public Symbol coerceSymbol (final Object arg, final boolean errorp)
    {
	if (arg != null)
	{
	    if (arg instanceof Symbol)
	    {
		return (Symbol)arg;
	    }
	}
	if (errorp)
	{
	    throw new IllegalArgumentException ("Can't coerce object to Symbol: " + arg);
	}
	return null;
    }

    public Integer coerceInteger (final Object arg, final boolean errorp)
    {
	if (arg != null)
	{
	    if (arg instanceof Integer)
	    {
		return (Integer)arg;
	    }
	}
	if (errorp)
	{
	    throw new IllegalArgumentException ("Can't coerce object to Integer: " + arg);
	}
	return null;
    }

    public Package coercePackage (final Object arg, final boolean errorp)
    {
	if (arg != null)
	{
	    if (arg instanceof Package)
	    {
		return (Package)arg;
	    }
	    final String packageName = coerceString (arg, false);
	    if (packageName != null)
	    {
		return PackageFactory.getPackage (packageName);
	    }
	}
	if (errorp)
	{
	    throw new IllegalArgumentException ("Can't coerce object to Package: " + arg);
	}
	return null;
    }

    public Object getObject (final List<Object> arguments, final int i)
    {
	return arguments.get (i);
    }

    public String getString (final List<Object> arguments, final int i)
    {
	final Object arg = arguments.get (i);
	return coerceString (arg, true);
    }

    public Symbol getSymbol (final List<Object> arguments, final int i)
    {
	final Object arg = arguments.get (i);
	return coerceSymbol (arg, true);
    }

    public int getInt (final List<Object> arguments, final int i)
    {
	final Object arg = arguments.get (i);
	return coerceInteger (arg, true);
    }

    public Package getPackage (final List<Object> arguments, final int i)
    {
	final Object arg = arguments.get (i);
	return coercePackage (arg, true);
    }

    /** Package functions are defined in. */
    public Package getPackage ()
    {
	return pkg;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (pkg);
	buffer.append (">");
	return buffer.toString ();
    }
}
