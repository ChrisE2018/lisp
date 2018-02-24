
package lisp;

public class CharAtom implements Lisp
{
    private final char value;

    public CharAtom (final char value)
    {
	this.value = value;
    }

    public char getValue ()
    {
	return this.value;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	buffer.append ("#\\");
	// [TODO] Translate char values to char names
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
