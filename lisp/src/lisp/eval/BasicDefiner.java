
package lisp.eval;

import java.lang.reflect.Method;
import java.util.List;

import lisp.*;
import lisp.Package;

public class BasicDefiner
{
    private final Object source;

    public BasicDefiner (final Object source)
    {
	this.source = source;
    }

    public BasicDefiner ()
    {
	source = this;
    }

    public Object getSource ()
    {
	return source;
    }

    /**
     * Bind a symbol to a java implementation method. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbol
     * @param methodName
     */
    public void define (final Symbol symbol, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new StandardFunctionCell (source, method));
    }

    /**
     * Bind a symbol to a java implementation method. <br>
     * The method must be a unique local method of the source class and the number and type of
     * arguments come from it.
     *
     * @param symbol
     * @param methodName
     */
    public void defineTyped (final Symbol symbol, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Method method = getNamedMethod (methodName);
	symbol.setFunction (new TypedFunctionCell (source, method));
    }

    private Method getNamedMethod (final String methodName) throws NoSuchMethodException
    {
	Method result = null;
	final Method[] methods = source.getClass ().getDeclaredMethods ();
	for (final Method method : methods)
	{
	    if (method.getName ().equals (methodName))
	    {
		if (result != null)
		{
		    final String cname = source.getClass ().getSimpleName ();
		    throw new NoSuchMethodException (cname + " has more than one method named " + methodName);
		}
		result = method;
	    }
	}
	if (result == null)
	{
	    final String cname = source.getClass ().getSimpleName ();
	    throw new NoSuchMethodException (cname + " has more no method named " + methodName);

	}
	return result;
    }

    /**
     * Bind a symbol to a java implementation method for a special form. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbol
     * @param methodName
     */
    public void defspecial (final Symbol symbol, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Method method = source.getClass ().getMethod (methodName, Interpreter.class, List.class);
	symbol.setFunction (new SpecialFunctionCell (source, method));
    }

    /**
     * Bind a symbol to a java implementation method for a macro. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbol
     * @param methodName
     */
    public void defmacro (final Symbol symbol, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new MacroFunctionCell (source, method));
    }

    public String coerceString (final Object arg)
    {
	return coerceString (arg, true);
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

    public Symbol coerceSymbol (final Object arg)
    {
	return coerceSymbol (arg, true);
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

    public Package coercePackage (final Object arg)
    {
	return coercePackage (arg, true);
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

    public Object coerceToParameter (final Class<?> p, final Object arg)
    {
	final Class<?> argClass = arg.getClass ();
	if (p.isAssignableFrom (argClass))
	{
	    return arg;
	}
	if (p == String.class)
	{
	    // Handle String from Symbol or String
	    if (arg instanceof Symbol)
	    {
		return ((Symbol)arg).getName ();
	    }
	    if (arg instanceof String)
	    {
		return arg;
	    }
	}
	// [TODO] Handle char, long, short etc.
	if (p == int.class || p == Integer.class)
	{
	    // Handle int from Lisp int
	    if (arg instanceof Integer)
	    {
		return arg;
	    }
	}
	if (p == double.class || p == Double.class)
	{
	    if (arg instanceof Double)
	    {
		return arg;
	    }
	}
	if (p == boolean.class || p == Boolean.class)
	{
	    if (arg instanceof Boolean)
	    {
		return arg;
	    }
	}
	throw new CoerceError ("Can't coerce " + arg + " to " + p);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (source);
	buffer.append (">");
	return buffer.toString ();
    }
}
