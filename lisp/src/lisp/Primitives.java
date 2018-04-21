
package lisp;

import java.util.*;

public class Primitives extends Definer
{
    public static void initialize () throws NoSuchMethodException, SecurityException
    {
	final Primitives primitives = new Primitives ();
	primitives.definePrimitives ();
    }

    private Primitives ()
    {
	super (PackageFactory.getSystemPackage ());
    }

    private void definePrimitives () throws NoSuchMethodException, SecurityException
    {
	defspecial ("quote", "quoteEvaluator");
	defspecial ("def", "defEvaluator");
	define ("list", "listEvaluator");
	define ("plus", "plusEvaluator");
	define ("+", "plusEvaluator");
	define ("times", "timesEvaluator");
	define ("*", "timesEvaluator");
	define ("in-package", "inPackageEvaluator");
	define ("java", "javaEvaluator");
    }

    public Object quoteEvaluator (final List<Object> arguments)
    {
	final Object result = arguments.get (1);
	return result;
    }

    public Object defEvaluator (final List<Object> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (1), true);
	final LispList arglist = (LispList)arguments.get (2);
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Object a : arglist)
	{
	    params.add ((Symbol)a);
	}
	final List<Object> body = new ArrayList<Object> ();
	for (int i = 3; i < arguments.size (); i++)
	{
	    body.add (arguments.get (i));
	}
	final DefFunctionCell function = new DefFunctionCell (name, params, body);
	name.setFunction (function);
	return name;
    }

    public Object listEvaluator (final List<Object> arguments)
    {
	return new LispParenList (arguments);
    }

    public Object plusEvaluator (final List<Object> arguments)
    {
	int result = 0;
	double dresult = 0;
	boolean integer = true;
	for (final Object a : arguments)
	{
	    if (!(a instanceof NumberAtom))
	    {
		throw new IllegalArgumentException ("Number required " + a);
	    }
	    final NumberAtom na = (NumberAtom)a;
	    if (na.isInteger ())
	    {
		result += na.getInteger ();
	    }
	    else
	    {
		integer = false;
		dresult += na.getFloat ();
	    }
	}
	if (integer)
	{
	    return new IntAtom (result);
	}
	return new DoubleAtom (result + dresult);
    }

    public Object timesEvaluator (final List<Object> arguments)
    {
	int result = 1;
	double dresult = 1;
	boolean integer = true;
	for (final Object a : arguments)
	{
	    if (!(a instanceof NumberAtom))
	    {
		throw new IllegalArgumentException ("Number required " + a);
	    }
	    final NumberAtom na = (NumberAtom)a;
	    if (na.isInteger ())
	    {
		result *= na.getInteger ();
	    }
	    else
	    {
		integer = false;
		dresult += na.getFloat ();
	    }
	}
	if (integer)
	{
	    return new IntAtom (result);
	}
	return new DoubleAtom (result * dresult);
    }

    public Object inPackageEvaluator (final List<Object> arguments)
    {
	final Package pkg = coercePackage (arguments.get (0), true);
	PackageFactory.setDefaultPackage (pkg);
	return pkg;
    }

    public Object javaEvaluator (final List<Object> arguments)
    {
	final Object target = getObject (arguments, 0);
	final String method = coerceString (arguments.get (1), true);
	// [TODO] Retrieve object methods.
	// [TODO] Scan arguments and coerse to valid types.
	// [TODO] Allow non Lisp return value.
	// [TODO] Define support functions to help get the right type from an argument list.
	return null;
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
