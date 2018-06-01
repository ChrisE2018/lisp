
package lisp.symbol;

import lisp.*;

public interface LispVisitor
{
    /** Call this at the start of each expression before doing anything else. */
    public void visitStart (LispList expression);

    /** Call this at the end of each expression after doing everything else. */
    public void visitEnd (LispList expression);

    /** Visit a sub-expression whose value may become the return value. */
    public void visitValue (Object expression);

    /**
     * Visit a sub-expression whose value will be used as a conditional test and may be returned.
     * Non-boolean types are treated as true for the conditional.
     */
    public void visitBooleanValue (Object expression);

    /**
     * Visit a sub-expression whose value will be used as a conditional test. Only boolean types
     * allowed.
     */
    public void visitBoolean (Object expression);

    /**
     * Visit a sub-expression whose value will be used as an integer. Only int types and directly
     * assignable types allowed.
     */
    public void visitInteger (Object expression);

    /** Visit a sub-expression whose value will be ignored. */
    public void visitIgnored (Object expression);

    /** Visit a sub-expression which will be returned as a value. */
    public void visitConstantValue (Object object);

    /** Set the value of a symbol. */
    public void visitSymbolSet (Symbol object);
}
