
package lisp.symbol;

import java.lang.reflect.Method;
import java.util.List;

import org.objectweb.asm.tree.ClassNode;

import lisp.Symbol;
import lisp.eval.LexicalContext;
import lisp.util.MultiMap;

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
    }

    @Override
    public void overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
    {
	throw new UnsupportedOperationException ("Can't overload interpreted function definitions");
    }

    @Override
    public Object eval (final LexicalContext context, final List<? extends Object> form) throws Exception
    {
	if (form.size () != arglist.size () + 1)
	{
	    throw new Error ("Incorrect argument count for " + name + ". Actual arg count " + (form.size () - 1)
	                     + " does not match " + arglist.size ());
	}
	final LexicalContext con = new LexicalContext (context);
	for (int i = 1; i < form.size (); i++)
	{
	    final Symbol var = arglist.get (i - 1);
	    con.bind (var, context.eval (form.get (i)));
	}
	Object result = null;
	// Evaluate the method body
	for (final Object f : body)
	{
	    result = con.eval (f);
	}
	return result;
    }

    // @Override
    // public Object apply (final Object... arguments) throws Exception
    // {
    // final LexicalContext context = new LexicalContext
    // (LexicalContext.getCurrentThreadLexicalContext ());
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Symbol var = arglist.get (i);
    // context.bind (var, arguments[i]);
    // }
    // Object result = null;
    // // Evaluate the method body
    // for (final Object f : body)
    // {
    // result = context.eval (f);
    // }
    // return result;
    // }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    @Override
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	super.getDescriberValues (result, target);
	result.put ("name", name);
	result.put ("arglist", arglist);
	result.put ("body", body);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (" ");
	buffer.append (name);
	buffer.append (">");
	return buffer.toString ();
    }
}
