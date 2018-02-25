
package lisp;

public abstract class NumberAtom extends Atom
{
    public boolean isInteger ()
    {
	return false;
    }

    public int getInteger ()
    {
	return 0;
    }

    public boolean isFloat ()
    {
	return false;
    }

    public double getFloat ()
    {
	return 0;
    }

    @Override
    public boolean isNumber ()
    {
	return true;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
