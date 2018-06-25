
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class PlusOneIntFunction implements Opcodes, LispTreeFunction
{
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults rs = context.compile (expression.get (1), true);
	context.convert (rs, int.class, false, false);
	final LabelNode l1 = new LabelNode ();
	context.add (new InsnNode (ICONST_1));
	context.add (new InsnNode (IADD));
	context.add (new JumpInsnNode (GOTO, l1));
	return new CompileResults (new ExplicitResult (l1, int.class));
    }
}
