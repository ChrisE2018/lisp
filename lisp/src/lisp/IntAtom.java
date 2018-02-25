
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

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (value);
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

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (value);
	buffer.append (">");
	return buffer.toString ();
    }
}
