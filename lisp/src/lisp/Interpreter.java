
package lisp;

import java.lang.reflect.Method;
import java.util.List;

public class Interpreter
{
    private final Package pkg = PackageFactory.getPackage ("default");

    public Interpreter () throws NoSuchMethodException, SecurityException
    {
	define ("list", "listEvaluator");
	define ("plus", "plusEvaluator");
    }

    private void define (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new StandardFunctionCell (this, method));
    }

    public Lisp listEvaluator (final List<Lisp> arguments)
    {
	return new LispParenList (arguments);
    }

    public Lisp plusEvaluator (final List<Lisp> arguments)
    {
	int sum = 0;
	for (final Lisp a : arguments)
	{
	    if (a instanceof IntAtom)
	    {
		final IntAtom i = (IntAtom)a;
		sum += i.getValue ();
	    }
	    else
	    {
		throw new IllegalArgumentException ("Integer required " + a);
	    }
	}
	return new IntAtom (sum);
    }

    public Lisp eval (final Lisp form) throws Exception
    {
	if (form instanceof Symbol)
	{
	    final Symbol symbol = (Symbol)form;
	    final Lisp result = symbol.getValue ();
	    return result;
	}
	if (form instanceof LispList)
	{
	    final LispList list = (LispList)form;
	    if (list.size () == 0)
	    {
		return form;
	    }
	    final Lisp fn = list.get (0);
	    if (!(fn instanceof Symbol))
	    {
		throw new IllegalArgumentException ("Function name required " + fn);
	    }
	    final Symbol f = (Symbol)fn;
	    final FunctionCell function = f.getFunction ();
	    if (function == null)
	    {
		throw new IllegalArgumentException ("Undefined function " + f);
	    }
	    System.out.printf ("Eval %s%n", form);
	    final Lisp result = function.eval (this, list);
	    return result;
	}
	return form;
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
