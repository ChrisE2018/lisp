
package lisp.cc4;

import lisp.LispList;
import lisp.symbol.LispVisitor;

public interface LispTreeWalker
{
    public static CompileResultSet VOID_RETURN = null;

    /** Call visitor on all directly nested subexpressions. */
    public void walker (LispVisitor visitor, final LispList expression);
}
