
package lisp.eval;

import java.lang.reflect.*;
import java.util.*;
import java.util.logging.Logger;

import lisp.*;
import lisp.Package;
import lisp.symbol.*;

/**
 * Base class for classes that contain Lisp primitives. This class can scan for methods with
 * DefineLisp annotations and make them into functions.
 *
 * @author cre
 */
public class Definer
{
    private static final Logger LOGGER = Logger.getLogger (Definer.class.getName ());

    /**
     * Keep track of Definer classes that have already been processed. Once a class has been
     * processed it will not be scanned again. This allows Definer classes to be instantiated more
     * than once.
     */
    private static Set<Class<?>> scannedDefiners = new HashSet<Class<?>> ();

    /**
     * The object bound to the defined methods. Normally the same as this but not required to be.
     */
    private final Object source;

    public Definer (final Object source)
    {
	this.source = source;
	getAnnotations (source);
    }

    public Definer ()
    {
	source = this;
	getAnnotations (source);
    }

    /**
     * The object bound to the defined methods. Normally the same as 'this' but not required to be.
     */
    public Object getSource ()
    {
	return source;
    }

    /** Scan an object class for DefineLisp annotations if not done already. */
    private void getAnnotations (final Object object)
    {
	final Class<?> objectClass = object.getClass ();
	if (!scannedDefiners.contains (objectClass))
	{
	    scannedDefiners.add (objectClass);
	    LOGGER.info (String.format ("Processing annotations for %s", object));
	    for (final Method method : objectClass.getDeclaredMethods ())
	    {
		if (method.isAnnotationPresent (DefineLisp.class))
		{
		    defineMethodFunction (object, method);
		}
	    }
	}
    }

    /** Make a method into a Lisp function after a DefineLisp annotation is found. */
    private void defineMethodFunction (final Object object, final Method method)
    {
	final DefineLisp a = method.getAnnotation (DefineLisp.class);
	final String packageName = a.packageName ();
	final Package p = PackageFactory.getPackage (packageName);
	final String className = a.classname ();
	final boolean special = a.special ();
	final boolean macro = a.macro ();
	String symbolName = a.name ();
	final String documentation = a.documentation ();
	if (symbolName.isEmpty ())
	{
	    // Default if the annotation does not specify the name is to use the name of the method
	    symbolName = method.getName ();
	}
	LOGGER.info (String.format ("define %s as %s", symbolName, method));
	final Symbol symbol = p.internSymbol (symbolName);

	// Overloading requires adding the method to an existing function cell
	if (macro)
	{
	    final FunctionCell function = new MacroFunctionCell (symbol, object, method, documentation);
	    symbol.setFunction (function);
	}
	else
	{
	    FunctionCell function = symbol.getFunction ();
	    if (function == null)
	    {
		if (special)
		{
		    function = new SpecialFunctionCell (symbol);
		    symbol.setFunction (function);
		}
		else
		{
		    function = new StandardFunctionCell (symbol);
		    symbol.setFunction (function);
		}
	    }
	    function.overload (object, method, documentation, null, null);
	}

	if (!className.isEmpty ())
	{
	    try
	    {
		final Class<?> fnClass = Class.forName (className);
		if (LispFunction.class.isAssignableFrom (fnClass))
		{
		    final Class<? extends LispFunction> functionClass = fnClass.asSubclass (LispFunction.class);
		    final Constructor<? extends LispFunction> constructor = functionClass.getConstructor ();
		    final FunctionCell function = symbol.getFunction ();
		    final LispFunction lispFunction = constructor.newInstance ();
		    function.setLispFunction (lispFunction);
		}
	    }
	    catch (final ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException
	            | IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    /** Make an Object into a String. Convenience method to simplify function definition. */
    public String coerceString (final Object arg)
    {
	return coerceString (arg, true);
    }

    /** Make an Object into a String. Convenience method to simplify function definition. */
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
	    throw new CoerceError ("Can't coerce object to String: %s", arg);
	}
	return null;
    }

    /** Make an Object into a Symbol. Convenience method to simplify function definition. */
    public Symbol coerceSymbol (final Object arg)
    {
	return coerceSymbol (arg, true);
    }

    /** Make an Object into a Symbol. Convenience method to simplify function definition. */
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
	    throw new CoerceError ("Can't coerce object to Symbol: %s", arg);
	}
	return null;
    }

    /** Make an Object into an Integer. Convenience method to simplify function definition. */
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
	    throw new CoerceError ("Can't coerce object to Integer: %s", arg);
	}
	return null;
    }

    /** Make an Object into a Package. Convenience method to simplify function definition. */
    public Package coercePackage (final Object arg)
    {
	return coercePackage (arg, true);
    }

    /** Make an Object into a Package. Convenience method to simplify function definition. */
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
	    throw new CoerceError ("Can't coerce object to Package: %s", arg);
	}
	return null;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (" ");
	buffer.append (source);
	buffer.append (">");
	return buffer.toString ();
    }
}
