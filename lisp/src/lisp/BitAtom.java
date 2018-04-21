
package lisp;

public class BitAtom extends NumberAtom
{
    private final int value;

    public BitAtom (final int value)
    {
	if (value != 0 && value != 1)
	{
	    throw new IllegalArgumentException ("Binary value required");
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
