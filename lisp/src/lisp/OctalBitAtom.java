
package lisp;

public class OctalBitAtom implements Lisp
{
    private final int value;

    public OctalBitAtom (final int value)
    {
	if (value < 0 || value > 7)
	{
	    throw new IllegalArgumentException ("Octal value required");
	}
	this.value = value;
    }

    public int getValue ()
    {
	return this.value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (this.value);
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
