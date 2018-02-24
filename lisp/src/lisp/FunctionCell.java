
package lisp;

public abstract class FunctionCell
{
    abstract public Lisp eval (final Interpreter interpreter, final LispList form) throws Exception;

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
