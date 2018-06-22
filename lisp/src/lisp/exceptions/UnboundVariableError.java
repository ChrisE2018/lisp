
package lisp.exceptions;

import lisp.lang.Symbol;

public class UnboundVariableError extends Error
{
    private final Symbol variable;

    public UnboundVariableError (final Symbol variable)
    {
	super (String.format ("Unbound global %s", variable));
	this.variable = variable;
    }

    public UnboundVariableError (final Symbol variable, final String format, final Object... args)
    {
	super (String.format (format, args));
	this.variable = variable;
    }

    public Symbol getVariable ()
    {
	return variable;
    }

    // @Override
    // public String toString ()
    // {
    // final StringBuilder buffer = new StringBuilder ();
    // buffer.append ("#<");
    // buffer.append (getClass ().getSimpleName ());
    // buffer.append (" ");
    // buffer.append (variable);
    // buffer.append (">");
    // return buffer.toString ();
    // }
}
