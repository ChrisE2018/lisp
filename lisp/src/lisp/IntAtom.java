
package lisp;

public class IntAtom extends NumberAtom
{
    private final int value;

    public IntAtom (final int value)
    {
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
