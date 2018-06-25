
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class RemLongFunction implements LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults rs1 = context.compile (expression.get (1), true);
	context.convert (rs1, long.class, false, false);
	final CompileResults rs2 = context.compile (expression.get (2), true);
	context.convert (rs2, long.class, false, false);
	context.add (new InsnNode (Opcodes.LREM));
	final LabelNode l1 = new LabelNode ();
	return new CompileResults (new ExplicitResult (l1, long.class));
    }
}
