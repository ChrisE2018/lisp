
package lisp.eval;

import java.util.*;

import lisp.Symbol;

/**
 * Combination of interpreter and lexical bindings.
 *
 * @author cre
 */
public class LexicalContext
{
    // All special functions should use this to implement bindings.
    private final Interpreter interpreter;

    private final Map<Symbol, Object> bindings;

    public LexicalContext (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
	bindings = new HashMap<Symbol, Object> ();
    }

    public LexicalContext (final LexicalContext context)
    {
	interpreter = context.interpreter;
	bindings = new HashMap<Symbol, Object> (context.bindings);
    }

    // public LexicalContext (final Interpreter interpreter, final Map<Symbol, Object> bindings)
    // {
    // this.interpreter = interpreter;
    // this.bindings = new HashMap<Symbol, Object> (bindings);
    // }

    public Interpreter getInterpreter ()
    {
	return interpreter;
    }

    public void set (final Symbol symbol, final Object value)
    {
	bindings.put (symbol, value);
    }

    public Object get (final Symbol symbol)
    {
	if (bindings.containsKey (symbol))
	{
	    return bindings.get (symbol);
	}
	return symbol.getValue ();
    }

    public Object get (final Symbol symbol, final Object defaultValue)
    {
	if (bindings.containsKey (symbol))
	{
	    return bindings.get (symbol);
	}
	return symbol.getValue (defaultValue);
    }

    public Object eval (final Object form) throws Exception
    {
	return interpreter.eval (this, form);
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
