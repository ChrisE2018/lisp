
package lisp;

public class CharAtom
{
    private final char value;

    public CharAtom (final char value)
    {
	this.value = value;
    }

    public char getValue ()
    {
	return value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (toString ());
    }

    @Override
    public String toString ()
    {
	// [TODO] Translate char values to char names
	return "#\\" + value;
    }
}
