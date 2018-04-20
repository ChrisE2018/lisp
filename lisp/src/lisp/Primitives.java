
package lisp;

import java.util.*;

public class Primitives
{
    public Primitives () throws NoSuchMethodException, SecurityException
    {
	final Definer definer = new Definer (PackageFactory.getSystemPackage (), this);
	definer.defspecial ("quote", "quoteEvaluator");
	definer.defspecial ("def", "defEvaluator");
	definer.define ("list", "listEvaluator");
	definer.define ("plus", "plusEvaluator");
	definer.define ("+", "plusEvaluator");
	definer.define ("times", "timesEvaluator");
	definer.define ("*", "timesEvaluator");
    }

    public Lisp quoteEvaluator (final List<Lisp> arguments)
    {
	final Lisp result = arguments.get (1);
	return result;
    }

    public Lisp defEvaluator (final List<Lisp> arguments)
    {
	final Symbol name = (Symbol)arguments.get (1);
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
