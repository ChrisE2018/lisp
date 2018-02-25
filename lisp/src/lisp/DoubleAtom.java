
package lisp;

public class DoubleAtom extends NumberAtom
{
    private final double value;

    public DoubleAtom (final double value)
    {
	this.value = value;
    }

    public double getValue ()
    {
	return value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append (value);
    }

    @Override
    public boolean isFloat ()
    {
	return true;
    }

    @Override
    public double getFloat ()
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
