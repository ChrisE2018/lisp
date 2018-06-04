
package lisp.cc4;

import lisp.LispList;

public interface LispTreeFunction
{
    /**
     * Compile an expression.
     *
     * @param context The compiler to use for subexpressions.
     * @param expression The special form to compile.
     * @param resultDesired Will the result be used after returning.
     * @return The class of the result produced.
     */
    public Class<?> compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired);
}
