
package lisp.eval;

import lisp.LispList;

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
public class ListPrimitives extends Definer
{
    // [TODO] Create immutable list
    // Property list access
    // [TODO] Maps, sets, union, intersection, difference
    // [TODO] Package creation, uses, export, import
    // [TODO] File functions

    @DefineLisp
    public Object list (final Object... arguments)
    {
	return new LispList (arguments);
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
