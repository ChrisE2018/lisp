
package lisp.cc4;

import lisp.LispList;
import lisp.symbol.LispFunction;

public interface LispTreeFunction extends LispFunction
{
    /**
     * Compile an expression.
     *
     * @param context The compiler to use for subexpressions.
     * @param expression The special form to compile. This can also be used to produce optimized
     *            code for normal functions.
     * @param resultDesired Will the result be used after returning. The compiler may avoid actions
     *            that would load data only to be discarded if this is false.
     * @return A description of the possible result types. Each entry describes an implicit or
     *         explicit result produced by the compiled expression. The compiled code must jump to
     *         the labels in the CompileResult and should not add them to the code. The calling code
     *         is responsible for inserting the labels at a position where the specific result can
     *         be handled. </br>
     *         ExplicitCompileResult objects indicate that the value class will be present on the
     *         stack when the label is jumped to. </br>
     *         ImplicitCompileResult objects indicate that a specific value should be assumed by
     *         code that gets jumped to. The value is not actually on the stack. A typical implicit
     *         result is true, false, null or a number. Quoted constants can also be returned as
     *         implicit results. The consumer can perform constant folding on the implicit results
     *         to avoid runtime instructions.
     */
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired);
}
