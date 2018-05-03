
package lisp.eval;

import java.util.*;
import java.util.Map.Entry;

import lisp.Symbol;

/** Function cell for a function definition of a named function that binds arguments to values. */
public class DefFunctionCell extends FunctionCell
{
    private final Symbol name;
    private final List<Symbol> arglist;
    private final List<Object> body;

    public DefFunctionCell (final Symbol name, final List<Symbol> arglist, final List<Object> body)
    {
	this.name = name;
	this.arglist = arglist;
	this.body = body;
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final List<Object> arguments = new ArrayList<Object> ();
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments.add (interpreter.eval (f));
	}
	Object result = null;
	final Map<Symbol, Object> savedValues = new HashMap<Symbol, Object> ();
	try
	{
	    // Bind arguments to arglist
	    // [TODO] This is not thread safe because the global symbol value is visible.
	    // Should use a hashmap type binding environment to keep global symbol value clean.
	    // That requires changing the interpreter eval function to accept and use a binding
	    // environment.
	    for (int i = 0; i < arglist.size (); i++)
	    {
		final Symbol arg = arglist.get (i);
		savedValues.put (arg, arg.getValue ());
		arg.setValue (arguments.get (i));
	    }
	    // Evaluate the method body
	    for (final Object f : body)
	    {
		result = interpreter.eval (f);
	    }
	}
	finally
	{
	    for (final Entry<Symbol, Object> entry : savedValues.entrySet ())
	    {
		entry.getKey ().setValue (entry.getValue ());
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
	buffer.append (" ");
	buffer.append (name);
	buffer.append (">");
	return buffer.toString ();
    }
}