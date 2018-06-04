
package lisp.cc4;

import java.util.Map;

import org.objectweb.asm.tree.InsnList;

import lisp.*;
import lisp.cc.LocalBinding;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public interface LispTreeFunction
{

    /** Call visitor on all directly nested subexpressions. */
    abstract public void walker (LispVisitor visitor, final LispList expression);

    /**
     * Attach bytecode instructions to instruction list
     *
     * @param compiler The compiler to use for subexpressions.
     * @param il The instruction list.
     * @param locals Local variable binding information.
     * @param expression The special form to compile.
     * @param resultDesired Will the result be used after returning.
     * @return The class of the result produced.
     */
    // abstract
    default public Class<?> compile (final TreeCompiler compiler, final InsnList il, final Map<Symbol, LocalBinding> locals,
            final LispList expression, final boolean resultDesired)
    {
	throw new UnsupportedOperationException ("Can't compile special form " + expression.head ());
    }

    default public Class<?> compile (final TreeCompilerContext treeCompilerContext, final LispList expression,
            final boolean resultDesired)
    {
	throw new UnsupportedOperationException ("Can't compile special form " + expression.head ());
    }
}
