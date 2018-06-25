
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.JumpInsnNode;
import lisp.cc.BlockBinding;
import lisp.cc4.*;
import lisp.lang.*;

public class ReturnFromFunction implements Opcodes, LispTreeFunction
{

    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo () (block-named foo (return-from foo 3) 4))
	final Symbol name = (Symbol)expression.get (1);
	final BlockBinding bb = context.getBlockBinding (name);
	if (bb == null)
	{
	    throw new Error ("There is no lexically visible block named " + name);
	}
	final Object expr = expression.get (2);
	final CompileResults rs = context.compile (expr, true);
	context.convert (rs, Object.class, false, false);
	context.add (new JumpInsnNode (GOTO, bb.getLabel ()));
	return null;
    }
    //
    // public CompileResultSet compilex (final TreeCompilerContext context, final LispList
    // expression, final boolean resultDesired)
    // {
    // // (define foo (x) (return-from bar x))
    // final Symbol name = (Symbol)expression.get (1);
    // final Object expr = expression.get (2);
    // context.add (new TypeInsnNode (NEW, "lisp/special/ReturnThrow"));
    // context.add (new InsnNode (DUP));
    //
    // final LabelNode l1 = new LabelNode ();
    // context.add (new ImplicitCompileResult (l1, name), Symbol.class);
    // context.add (l1);
    //
    // final CompileResultSet rs = context.compile (expr, true);
    // context.convert (rs, Object.class, false, false);
    // context.add (new MethodInsnNode (INVOKESPECIAL, "lisp/special/ReturnThrow", "<init>",
    // "(Llisp/lang/Symbol;Ljava/lang/Object;)V", false));
    // context.add (new InsnNode (ATHROW));
    //
    // return context.compile (expression.last (), true);
    // }
}
