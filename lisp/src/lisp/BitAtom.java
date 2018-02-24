
package lisp;

public class BitAtom implements Lisp
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
	return this.value;
    }

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
