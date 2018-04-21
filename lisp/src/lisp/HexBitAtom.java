
package lisp;

public class HexBitAtom extends NumberAtom
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
	return value;
    }

    @Override
    public boolean isInteger ()
    {
	return true;
    }

    @Override
    public int getInteger ()
    {
	return value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (value);
    }

    @Override
    public String toString ()
    {
	return String.valueOf (value);
    }
}
