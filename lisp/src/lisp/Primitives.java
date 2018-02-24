
package lisp;

import java.lang.reflect.Method;
import java.util.List;

public class Primitives
{
    private final Package pkg = PackageFactory.getPackage ("default");

    public Primitives () throws NoSuchMethodException, SecurityException
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
