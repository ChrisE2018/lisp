
package lisp;

public class HexBitAtom implements Lisp
{
    private final int value;

    public HexBitAtom (final int value)
    {
	if (value < 0 && value > 15)
	{
	    throw new IllegalArgumentException ("Hex value required");
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
