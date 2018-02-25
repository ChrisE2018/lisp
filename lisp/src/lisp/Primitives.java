
package lisp;

import java.lang.reflect.Method;
import java.util.*;

public class Primitives
{
    private final Package pkg = PackageFactory.getPackage ("default");

    public Primitives () throws NoSuchMethodException, SecurityException
    {
	defspecial ("def", "defEvaluator");
	define ("list", "listEvaluator");
	define ("plus", "plusEvaluator");
	define ("+", "plusEvaluator");
	define ("times", "timesEvaluator");
	define ("*", "timesEvaluator");
    }

    private void define (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new StandardFunctionCell (this, method));
    }

    private void defspecial (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new SpecialFunctionCell (this, method));
    }

    private void defmacro (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new MacroFunctionCell (this, method));
    }

    public Lisp defEvaluator (final List<Lisp> arguments)
    {
	final Symbol name = (Symbol)arguments.get (0);
	final Lisp arglist = arguments.get (1);
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Lisp a : (LispList)arglist)
	{
	    params.add ((Symbol)a);
	}
	final List<Lisp> body = new ArrayList<Lisp> ();
	for (int i = 2; i < arguments.size (); i++)
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
