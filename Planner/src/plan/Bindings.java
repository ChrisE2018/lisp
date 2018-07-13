
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

    // public Bindings replaceValues (final Bindings anonymousBindings)
    // {
    // Bindings result = this;
    // if (replaceValuesMustCopy (anonymousBindings))
    // {
    // result = new Bindings ();
    // for (final java.util.Map.Entry<Symbol, Symbol> entry : entrySet ())
    // {
    // final Symbol key = entry.getKey ();
    // final Symbol value = entry.getValue ();
    // if (anonymousBindings.containsKey (value))
    // {
    // result.put (key, anonymousBindings.get (value));
    // }
    // else
    // {
    // result.put (key, value);
    // }
    // }
    // }
    // return result;
    // }
    //
    // private boolean replaceValuesMustCopy (final Bindings anonymousBindings)
    // {
    // for (final Symbol s : values ())
    // {
    // if (anonymousBindings.containsKey (s))
    // {
    // return true;
    // }
    // }
    // return false;
    // }

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
