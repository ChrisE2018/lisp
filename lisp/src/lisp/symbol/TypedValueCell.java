
package lisp.symbol;

import javax.lang.model.type.NullType;

public class TypedValueCell implements ValueCell
{
    /** The current stored value. */
    private Object value;

    private final Class<?> type;

    public TypedValueCell (final Class<?> type)
    {
	this.type = type;
	value = null;
    }

    public TypedValueCell (final Class<?> type, final Object value)
    {
	this.type = type;
	this.value = value;
    }

    public Class<?> getType ()
    {
	return type;
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
	final Class<?> cls = (value == null) ? NullType.class : value.getClass ();
	if (!type.isAssignableFrom (cls))
	{
	    throw new IllegalArgumentException ("Object " + value + " cannot be assigned to " + type);
	}
	this.value = value;
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
