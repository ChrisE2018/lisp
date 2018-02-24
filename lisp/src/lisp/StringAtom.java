
package lisp;

public class StringAtom implements Lisp
{
    private final String value;

    public StringAtom (final String value)
    {
	this.value = value;
    }

    public String getValue ()
    {
	return this.value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append ('"');
	// [TODO] Slashify characters as required
	buffer.append (this.value);
	buffer.append ('"');
    }

    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (this.value);
	buffer.append (">");
	return buffer.toString ();
    }
}
