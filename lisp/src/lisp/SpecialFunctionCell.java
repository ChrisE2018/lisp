
package lisp;

public class SpecialFunctionCell extends FunctionCell
{

    @Override
    public Lisp eval (final Interpreter interpreter, final LispList form)
    {
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
