
package lisp;

public class StringAtom
{
    private final String value;

    public StringAtom (final String value)
    {
	this.value = value;
    }

    public String getValue ()
    {
	return value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append ('"');
	// [TODO] Slashify characters as required
	buffer.append (value);
	buffer.append ('"');
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }
}
