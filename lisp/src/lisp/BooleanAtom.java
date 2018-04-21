
package lisp;

public class BooleanAtom
{
    private final boolean value;

    public BooleanAtom (final boolean value)
    {
	this.value = value;
    }

    public boolean getValue ()
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
