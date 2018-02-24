
package lisp;

public class DoubleAtom implements Lisp
{
    private final double value;

    public DoubleAtom (final double value)
    {
	this.value = value;
    }

    public double getValue ()
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
