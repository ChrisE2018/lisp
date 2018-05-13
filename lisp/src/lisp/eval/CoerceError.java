
package lisp.eval;

public class CoerceError extends Error
{
    public CoerceError (final String format, final Object... args)
    {
	super (String.format (format, args));
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
