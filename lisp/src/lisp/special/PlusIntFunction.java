
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class PlusIntFunction implements Opcodes, LispTreeFunction
{
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// TODO Check if both rs1 and rs2 are implicit. If so, constant fold and return implicit.
	// (define double:foo (int:a double:b) (+ a b))
	// (define int:foo (short:a int:b) (+ a b))
	// (define int:foo (short:a byte:b) (+ a b))
	// (define double:foo (long:a double:b) (+ a b))
	// (define long:foo (long:a long:b) (+ a b))
	// (define double:foo (double:a int:b) (+ a b))
	final CompileResults rs1 = context.compile (expression.get (1), true);
	context.convert (rs1, int.class, false, false);
	final CompileResults rs2 = context.compile (expression.get (2), true);
	context.convert (rs2, int.class, false, false);
	context.add (new InsnNode (IADD));
	final LabelNode l1 = new LabelNode ();
	context.add (new JumpInsnNode (GOTO, l1));
	return new CompileResults (new ExplicitResult (l1, int.class));
    }
}
