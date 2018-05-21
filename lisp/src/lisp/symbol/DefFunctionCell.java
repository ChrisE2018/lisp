
package lisp.symbol;

import java.lang.reflect.Method;
import java.util.*;
import java.util.Map.Entry;

import lisp.Symbol;
import lisp.eval.LexicalContext;

/** Function cell for a function definition of a named function that binds arguments to values. */
public class DefFunctionCell extends FunctionCell
{
    private final Symbol name;
    private final List<Symbol> arglist;
    private final Object[] body;

    public DefFunctionCell (final Symbol name, final List<Symbol> arglist, final Object... body)
    {
	super (name, true);
	this.name = name;
	this.arglist = arglist;
	this.body = body;
	// [TODO] Partial or complete body compilation here.
    }

    @Override
    public void overload (final Object obj, final Method method)
    {
	throw new UnsupportedOperationException ("Can't overload interpreted function definitions");
    }

    @Override
    public Object eval (final LexicalContext context, final List<?> form) throws Exception
    {
	final List<Object> arguments = new ArrayList<Object> ();
	for (int i = 1; i < form.size (); i++)
	{
	    final Object f = form.get (i);
	    arguments.add (context.eval (f));
	}
	Object result = null;
	final Map<Symbol, ValueCell> savedValues = new HashMap<Symbol, ValueCell> ();
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
		savedValues.put (arg, arg.getValueCell ());
		arg.setValue (arguments.get (i));
	    }
	    // Evaluate the method body
	    for (final Object f : body)
	    {
		result = context.eval (f);
	    }
	}
	finally
	{
	    for (final Entry<Symbol, ValueCell> entry : savedValues.entrySet ())
	    {
		entry.getKey ().setValueCell (entry.getValue ());
	    }
	}
	return result;
    }

    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target
     * @return
     */
    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("name", name);
	result.put ("arglist", arglist);
	result.put ("body", body);
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
