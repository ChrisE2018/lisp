
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.*;

public class ReturnFromFunction implements Opcodes, LispTreeFunction
{
    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo (x) (return-from bar x))
	final Symbol name = (Symbol)expression.get (1);
	final Object expr = expression.get (2);
	context.add (new TypeInsnNode (NEW, "lisp/special/ReturnThrow"));
	context.add (new InsnNode (DUP));

	final LabelNode l1 = new LabelNode ();
	context.add (new ImplicitCompileResult (l1, name));
	context.add (l1);

	final CompileResultSet rs = context.compile (expr, true);
	context.convert (rs, Object.class, false, false);
	context.add (new MethodInsnNode (INVOKESPECIAL, "lisp/special/ReturnThrow", "<init>",
	        "(Llisp/lang/Symbol;Ljava/lang/Object;)V", false));
	context.add (new InsnNode (ATHROW));

	return context.compile (expression.last (), true);
    }
}
