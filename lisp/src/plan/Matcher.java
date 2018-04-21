
package plan;

import java.util.*;
import java.util.Map.Entry;

import lisp.*;

public class Matcher
{
    private static final char QUESTION_MARK = '?';

    public Map<Symbol, Symbol> match (final LispList pattern, final LispList literal)
    {
	if (pattern.size () != literal.size ())
	{
	    return null;
	}
	final Map<Symbol, Symbol> bindings = new HashMap<Symbol, Symbol> ();
	return match (pattern, literal, bindings);
    }

    public Map<Symbol, Symbol> match (final LispList pattern, final LispList literal, final Map<Symbol, Symbol> bindings)
    {
	if (pattern.size () != literal.size ())
	{
	    return null;
	}
	final Map<Symbol, Symbol> result = new HashMap<Symbol, Symbol> (bindings);
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

    private boolean isVariable (final Symbol symbol)
    {
	return symbol.getName ().charAt (0) == QUESTION_MARK;
    }

    public LispParenList bindingsToLisp (final Map<Symbol, Symbol> bindings)
    {
	LispParenList result = null;
	if (bindings != null)
	{
	    result = new LispParenList ();
	    for (final Entry<Symbol, Symbol> entry : bindings.entrySet ())
	    {
		final Symbol key = entry.getKey ();
		final Symbol value = entry.getValue ();
		final LispParenList slot = new LispParenList ();
		slot.add (key);
		slot.add (value);
		result.add (slot);
	    }
	}
	return result;
    }

    public LispParenList mapToLisp (final Map<Object, Object> bindings)
    {
	LispParenList result = null;
	if (bindings != null)
	{
	    result = new LispParenList ();
	    for (final Entry<Object, Object> entry : bindings.entrySet ())
	    {
		final Object key = entry.getKey ();
		final Object value = entry.getValue ();
		final LispParenList slot = new LispParenList ();
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
