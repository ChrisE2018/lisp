
package lisp;

public class FunctionCell
{
    public Lisp eval (final Interpreter interpreter, final LispList form) throws Exception
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
