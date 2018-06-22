
package lisp.eval;

import java.util.*;

import lisp.lang.Symbol;

/**
 * Combination of interpreter and lexical bindings.
 *
 * @author cre
 */
public class LexicalContext
{
    private static Map<Thread, LexicalContext> threadContextMap = new HashMap<Thread, LexicalContext> ();

    public static LexicalContext getCurrentThreadLexicalContext ()
    {
	return threadContextMap.get (Thread.currentThread ());
    }

    // All special functions should use this to implement bindings.
    private final Interpreter interpreter;

    private final Map<Symbol, Object> bindings;

    public LexicalContext (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
	bindings = new HashMap<Symbol, Object> ();
	threadContextMap.put (Thread.currentThread (), this);
    }

    public LexicalContext (final LexicalContext context)
    {
	interpreter = context.interpreter;
	bindings = new HashMap<Symbol, Object> (context.bindings);
	threadContextMap.put (Thread.currentThread (), this);
    }

    public Interpreter getInterpreter ()
    {
	return interpreter;
    }

    public void bind (final Symbol symbol, final Object value)
    {
	bindings.put (symbol, value);
    }

    public void set (final Symbol symbol, final Object value)
    {
	if (bindings.containsKey (symbol))
	{
	    bindings.put (symbol, value);
	}
	else
	{
	    symbol.setValue (value);
	}
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
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
