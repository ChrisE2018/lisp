
package lisp;

import java.util.*;
import java.util.Map.Entry;

public class DefFunctionCell extends FunctionCell
{
    private final Symbol name;
    private final List<Symbol> arglist;
    private final List<Lisp> body;

    public DefFunctionCell (final Symbol name, final List<Symbol> arglist, final List<Lisp> body)
    {
	this.name = name;
	this.arglist = arglist;
	this.body = body;
    }

    @Override
    public Lisp eval (final Interpreter interpreter, final LispList form) throws Exception
    {
	final List<Lisp> arguments = new ArrayList<Lisp> ();
	for (int i = 1; i < form.size (); i++)
	{
	    final Lisp f = form.get (i);
	    arguments.add (interpreter.eval (f));
	}
	Lisp result = null;
	final Map<Symbol, Lisp> savedValues = new HashMap<Symbol, Lisp> ();
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
	    for (final Lisp f : body)
	    {
		result = interpreter.eval (f);
	    }
	}
	finally
	{
	    for (final Entry<Symbol, Lisp> entry : savedValues.entrySet ())
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
