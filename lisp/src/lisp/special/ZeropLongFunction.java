
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class ZeropLongFunction implements LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults resultSet = context.compile (expression.get (1), true);
	// Do something with resultSet to throw away garbage if required
	context.convert (resultSet, long.class, false, false);
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	context.add (new JumpInsnNode (Opcodes.IFEQ, l1));
	context.add (new JumpInsnNode (Opcodes.GOTO, l2));
	return new CompileResults (new ImplicitResult (l1, true), new ImplicitResult (l2, false));
    }
}
