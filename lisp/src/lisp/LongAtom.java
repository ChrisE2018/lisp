
package lisp;

public class LongAtom implements Lisp
{
    private final long value;

    public LongAtom (final long value)
    {
	this.value = value;
    }

    public long getValue ()
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
