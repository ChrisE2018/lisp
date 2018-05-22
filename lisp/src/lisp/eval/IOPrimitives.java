
package lisp.eval;

public class IOPrimitives extends Definer
{
    @DefineLisp
    public Object printf (final String format, final Object... arguments)
    {
	System.out.printf (format, arguments);
	return null;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
