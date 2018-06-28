
package lisp.cc4;

import java.util.List;

import lisp.symbol.LispVisitor;

public interface LispTreeWalker
{
    public static CompileResults VOID_RETURN = null;

    /** Call visitor on all directly nested subexpressions. */
    public void walker (LispVisitor visitor, final List<?> expression);
}
