
package lisp;

public class BooleanAtom implements Lisp
{
    private final boolean value;

    public BooleanAtom (final boolean value)
    {
	this.value = value;
    }

    public boolean getValue ()
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
