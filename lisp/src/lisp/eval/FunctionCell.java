
package lisp.eval;

import java.util.List;

/** Base class of all function cells. */
public abstract class FunctionCell
{
    abstract public Object eval (final Interpreter interpreter, final List<?> form) throws Exception;

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
