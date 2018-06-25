
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class NotFunction implements Opcodes, LispTreeFunction
{
    /**
     * Compile negation into primitive bytecode. This eliminates one or two function calls, but
     * makes the bytecode bigger so the net effect may not be helpful. When everything is declared
     * boolean it does better.
     */
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults testResultSet = context.compile (expression.get (1), true);
	final LabelNode lFalse = new LabelNode (); // This label means we return false
	final LabelNode lTrue = new LabelNode (); // This label means we return true
	context.convertIfFalse (testResultSet, false, true, lTrue); // Note logic inversion
	context.add (new JumpInsnNode (GOTO, lFalse));
	final CompileResults result = new CompileResults ();
	result.addImplicitCompileResult (lFalse, false);
	result.addImplicitCompileResult (lTrue, true);
	return result;
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
