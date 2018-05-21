
package lisp.eval;

import java.util.*;

import lisp.*;
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
     * Implementation of function definition. This could compile the function body into bytecode. A
     * preliminary process would be code analysis to determine the lexical variable references and
     * replace these with a reference distinct from the global symbol reference.
     *
     * @param interpreter Not used, but required by calling protocol.
     */
    @DefineLisp (special = true, name = "def")
    public Object defEvaluator (final Interpreter interpreter, final Symbol name, final LispList arglist, final Object... body)
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

    @DefineLisp (special = true, name = "let")
    public Object letEvaluator (final Interpreter interpreter, final LispList arglist, final Object body1, final Object... body)
    {
	throw new Error ("Can't evaluate interpreted let form");
    }

    @DefineLisp (special = true, name = "let*")
    public Object letStarEvaluator (final Interpreter interpreter, final LispList arglist, final Object body1,
            final Object... body)
    {
	throw new Error ("Can't evaluate interpreted let* form");
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
