
package lisp.util;

import java.util.function.Supplier;

public class LogString implements Supplier<String>
{
    private final String format;
    private final Object[] args;

    public LogString (final String format, final Object... args)
    {
	this.format = format;
	this.args = args;
    }

    @Override
    public String get ()
    {
	return String.format (format, args);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
