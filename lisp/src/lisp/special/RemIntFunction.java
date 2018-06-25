
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class RemIntFunction implements LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults rs1 = context.compile (expression.get (1), true);
	context.convert (rs1, int.class, false, false);
	final CompileResults rs2 = context.compile (expression.get (2), true);
	context.convert (rs2, int.class, false, false);
	context.add (new InsnNode (Opcodes.IREM));
	final LabelNode l1 = new LabelNode ();
	return new CompileResults (new ExplicitResult (l1, int.class));
    }
}
