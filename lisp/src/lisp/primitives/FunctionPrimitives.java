
package lisp.primitives;

import java.util.*;

import lisp.eval.*;
import lisp.lang.*;
import lisp.symbol.DefFunctionCell;

/**
 * Bind java functions to lisp symbols.
 * <p>
 * Note that special forms like quote and setq are passed the interpreter as a first parameter and
 * the entire expression (including the function name) as the arguments. The real arguments start at
 * the second position. This is convenient because the interpreter is already processing the whole
 * expression and would have to allocate a new object to remove the function from the start of the
 * expression.
 * </p>
 * <p>
 * Normal functions are just passed the evaluated arguments. The real arguments start at the
 * beginning. This is convenient since the interpreter creates a new list for the arguments anyhow.
 * </p>
 */
public class FunctionPrimitives extends Definer
{
    /**
     * Implementation of function definition. The function definition is saved in source form so it
     * can be evaluated with the interpreter.
     *
     * @param context Required by calling convention but not used.
     * @param interpreter Not used, but required by calling protocol.
     */
    @DefineLisp (special = true, name = "def")
    public Object defEvaluator (final LexicalContext context, final Symbol name, final LispList arglist, final Object... body)
    {
	System.err.printf ("Warning: usage of obsolete def %s %n", name);
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Object a : arglist)
	{
	    params.add ((Symbol)a);
	}
	final DefFunctionCell function = new DefFunctionCell (name, params, body);
	name.setFunction (function);
	return name;
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
