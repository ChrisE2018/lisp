
package lisp.eval;

import java.util.List;

import lisp.Symbol;

/** Simple interpreter that using reflection to evaluate forms like Lisp functions. */
public class Interpreter
{
    public Object eval (final Object form) throws Exception
    {
	if (form instanceof Symbol)
	{
	    final Symbol symbol = (Symbol)form;
	    final Object result = symbol.getValue ();
	    return result;
	}
	if (form instanceof List<?>)
	{
	    final List<?> list = (List<?>)form;
	    if (list.size () == 0)
	    {
		return form;
	    }
	    final Object fn = list.get (0);
	    if (!(fn instanceof Symbol))
	    {
		throw new IllegalArgumentException ("Function is not a symbol " + fn);
	    }
	    final Symbol f = (Symbol)fn;
	    final FunctionCell function = f.getFunction ();
	    if (function == null)
	    {
		// [TODO] If function is bound to a java object, try to form a method call
		throw new IllegalArgumentException ("Symbol has no function definition " + f);
	    }
	    // System.out.printf ("Eval %s%n", form);
	    final Object result = function.eval (this, list);
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
