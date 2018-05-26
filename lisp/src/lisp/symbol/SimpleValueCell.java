
package lisp.symbol;

public class SimpleValueCell implements ValueCell
{
    /** The current stored value. */
    private Object value;

    public SimpleValueCell ()
    {
	value = null;
    }

    public SimpleValueCell (final Object value)
    {
	this.value = value;
    }

    /** Get the current stored value. */
    @Override
    public Object getValue ()
    {
	return value;
    }

    /** Change the current stored value. */
    @Override
    public void setValue (final Object value)
    {
	this.value = value;
    }

    /** Allowed value types. */
    public Class<?> getValueType ()
    {
	return Object.class;
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
