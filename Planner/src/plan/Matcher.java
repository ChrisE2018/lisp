
package plan;

import java.util.Map;
import java.util.Map.Entry;

import lisp.*;

public class Matcher
{
    private static final char QUESTION_MARK = '?';

    public static boolean isVariable (final Symbol symbol)
    {
	return symbol.getName ().charAt (0) == QUESTION_MARK;
    }

    public Bindings match (final LispList pattern, final LispList literal)
    {
	if (pattern.size () != literal.size ())
	{
	    return null;
	}
	final Bindings bindings = new Bindings ();
	return match (pattern, literal, bindings);
    }

    public Bindings match (final LispList pattern, final LispList literal, final Bindings bindings)
    {
	if (pattern.size () != literal.size ())
	{
	    return null;
	}
	final Bindings result = new Bindings (bindings);
	for (int i = 0; i < pattern.size (); i++)
	{
	    final Symbol symbol = (Symbol)pattern.get (i);
	    final Symbol term = (Symbol)literal.get (i);
	    if (isVariable (symbol))
	    {
		final Symbol binding = result.get (symbol);
		if (binding == null)
		{
		    result.put (symbol, term);
		}
		else if (binding != term)
		{
		    // Fail
		    return null;
		}
	    }
	    else if (symbol != term)
	    {
		// Fail
		return null;
	    }
	}
	return result;
    }

    public LispList bindingsToLisp (final Map<Symbol, Symbol> bindings)
    {
	LispList result = null;
	if (bindings != null)
	{
	    result = new LispList ();
	    for (final Entry<Symbol, Symbol> entry : bindings.entrySet ())
	    {
		final Symbol key = entry.getKey ();
		final Symbol value = entry.getValue ();
		final LispList slot = new LispList ();
		slot.add (key);
		slot.add (value);
		result.add (slot);
	    }
	}
	return result;
    }

    public LispList mapToLisp (final Map<Object, Object> bindings)
    {
	LispList result = null;
	if (bindings != null)
	{
	    result = new LispList ();
	    for (final Entry<Object, Object> entry : bindings.entrySet ())
	    {
		final Object key = entry.getKey ();
		final Object value = entry.getValue ();
		final LispList slot = new LispList ();
		slot.add (key);
		slot.add (value);
		result.add (slot);
	    }
	}
	return result;
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
