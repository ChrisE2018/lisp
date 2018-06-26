
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.JumpInsnNode;
import lisp.cc.BlockBinding;
import lisp.cc4.*;
import lisp.lang.LispList;

public class ReturnFunction implements Opcodes, LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo () (block (return 3) 4))
	final BlockBinding bb = context.getBlockBinding (null);
	if (bb == null)
	{
	    throw new Error ("There is no lexically visible anonymous block");
	}
	final Class<?> returnClass = bb.getReturnClass ();
	if (returnClass == void.class)
	{
	    // Return from constructor
	    context.add (new JumpInsnNode (GOTO, bb.getLabel ()));
	    return null;
	}
	else
	{
	    final Object expr = expression.get (1);
	    final CompileResults rs = context.compile (expr, true);
	    context.convert (rs, returnClass, false, false);
	    context.add (new JumpInsnNode (GOTO, bb.getLabel ()));
	    return null;
	}
    }
}
