
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

    public Lisp quoteEvaluator (final List<Lisp> arguments)
    {
	final Lisp result = arguments.get (1);
	return result;
    }

    public Lisp defEvaluator (final List<Lisp> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (1), true);
	final Lisp arglist = arguments.get (2);
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Lisp a : (LispList)arglist)
	{
	    params.add ((Symbol)a);
	}
	final List<Lisp> body = new ArrayList<Lisp> ();
	for (int i = 3; i < arguments.size (); i++)
	{
	    body.add (arguments.get (i));
	}
	final DefFunctionCell function = new DefFunctionCell (name, params, body);
	name.setFunction (function);
	return name;
    }

    public Lisp listEvaluator (final List<Lisp> arguments)
    {
	return new LispParenList (arguments);
    }

    public Lisp plusEvaluator (final List<Lisp> arguments)
    {
	int result = 0;
	double dresult = 0;
	boolean integer = true;
	for (final Lisp a : arguments)
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

    public Lisp timesEvaluator (final List<Lisp> arguments)
    {
	int result = 1;
	double dresult = 1;
	boolean integer = true;
	for (final Lisp a : arguments)
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

    public Lisp inPackageEvaluator (final List<Lisp> arguments)
    {
	final Package pkg = coercePackage (arguments.get (0), true);
	PackageFactory.setDefaultPackage (pkg);
	return pkg;
    }

    public Lisp javaEvaluator (final List<Object> arguments)
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
