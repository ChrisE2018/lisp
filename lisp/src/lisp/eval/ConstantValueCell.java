
package lisp.eval;

public class ConstantValueCell extends TypedValueCell
{
    public ConstantValueCell (final Class<?> type, final Object value)
    {
	super (type, value);
    }

    private ConstantValueCell (final Class<?> type)
    {
	super (type);
	setValue (null);
    }

    /** Change the current stored value. */
    @Override
    public void setValue (final Object value)
    {
	throw new UnsupportedOperationException ("Contant value cell cannot be changed");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getValue ());
	buffer.append (">");
	return buffer.toString ();
    }
}
