
package lisp;

public class Interpreter
{
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
