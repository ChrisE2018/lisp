
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class CompareLEIntFunction implements LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults rs1 = context.compile (expression.get (1), true);
	context.convert (rs1, int.class, false, false);
	final CompileResults rs2 = context.compile (expression.get (2), true);
	context.convert (rs2, int.class, false, false);
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	context.add (new JumpInsnNode (Opcodes.IF_ICMPLE, l1));
	context.add (new JumpInsnNode (Opcodes.GOTO, l2));
	return new CompileResults (new ImplicitResult (l1, true), new ImplicitResult (l2, false));
    }
}
