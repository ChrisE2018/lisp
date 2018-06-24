
package lisp.special;

import lisp.lang.Symbol;

public class ReturnThrow extends Error
{
    private final Symbol name;
    private final Object value;

    public ReturnThrow (final Symbol name, final Object value)
    {
	this.name = name;
	this.value = value;
    }

    public Symbol getName ()
    {
	return name;
    }

    public Object getValue ()
    {
	return value;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	buffer.append (" ");
	buffer.append (value);
	buffer.append (">");
	return buffer.toString ();
    }
}
