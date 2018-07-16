
package plan;

import java.util.LinkedHashMap;

import lisp.lang.Symbol;

/**
 * Bindings produced by matching Conditions. This just names a specification of HashMap.
 *
 * @author cre
 */
public class Bindings extends LinkedHashMap<Symbol, Symbol>
{
    public Bindings ()
    {
	super ();
    }

    public Bindings (final Bindings parent)
    {
	super (parent);
    }

    public Symbol translate (final Symbol symbol)
    {
	final Symbol result = get (symbol);
	if (result == null)
	{
	    return symbol;
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	for (final java.util.Map.Entry<Symbol, Symbol> entry : entrySet ())
	{
	    buffer.append (" ");
	    buffer.append (entry.getKey ());
	    buffer.append (": ");
	    buffer.append (entry.getValue ());
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
