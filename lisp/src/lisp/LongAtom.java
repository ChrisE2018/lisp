
package lisp;

public class LongAtom extends NumberAtom
{
    private final long value;

    public LongAtom (final long value)
    {
	this.value = value;
    }

    public long getValue ()
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
	return (int)value;
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
