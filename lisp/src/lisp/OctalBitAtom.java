
package lisp;

public class OctalBitAtom extends NumberAtom
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
