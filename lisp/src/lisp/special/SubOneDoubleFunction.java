
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class SubOneDoubleFunction implements Opcodes, LispTreeFunction
{
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults rs = context.compile (expression.get (1), true);
	context.convert (rs, double.class, false, false);
	final LabelNode l1 = new LabelNode ();
	context.add (new InsnNode (DCONST_1));
	context.add (new InsnNode (DSUB));
	context.add (new JumpInsnNode (GOTO, l1));
	return new CompileResults (new ExplicitResult (l1, double.class));
    }
}
