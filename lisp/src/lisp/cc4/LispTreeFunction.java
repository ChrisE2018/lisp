
package lisp.cc4;

import lisp.LispList;
import lisp.symbol.LispFunction;

public interface LispTreeFunction extends LispFunction
{
    /**
     * Compile an expression.
     *
     * @param context The compiler to use for subexpressions.
     * @param expression The special form to compile.
     * @param resultDesired Will the result be used after returning.
     * @return An array of objects describing the possible result types. Each row in the array
     *         starts with a LabelNode (or null) followed by either: 1) a constant value or 2) any
     *         number of Class<?> objects. If the row starts with null it describes the situation
     *         when the compiled code falls through to the end. It the row starts with a label then
     *         the compiled code jumps to that label (which is not part of the instruction list). If
     *         the row contains Class<?> objects then the top of the stack can contain any of those
     *         types. If the row contains a constant expression then that is an implied value of the
     *         compiled code, but it is not on the stack.
     */
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired);
}
