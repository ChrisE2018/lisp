
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class ReturnFunction implements Opcodes, LispTreeFunction
{
    // {
    // mv = cw.visitMethod(ACC_PUBLIC, "blockReturn",
    // "(Llisp/eval/LexicalContext;Ljava/lang/Object;)Ljava/lang/Object;", null, new String[] {
    // "java/lang/Exception", "lisp/special/ReturnThrow" });
    // {
    // av0 = mv.visitAnnotation("Llisp/eval/DefineLisp;", true);
    // av0.visit("special", Boolean.TRUE);
    // av0.visit("name", "return");
    // av0.visit("classname", "lisp.special.ReturnFunction");
    // av0.visitEnd();
    // }

    // mv.visitTypeInsn(NEW, "lisp/special/ReturnThrow");
    // mv.visitInsn(DUP);
    // mv.visitInsn(ACONST_NULL);
    // mv.visitVarInsn(ALOAD, 1);
    // mv.visitVarInsn(ALOAD, 2);
    // mv.visitMethodInsn(INVOKEVIRTUAL, "lisp/eval/LexicalContext", "eval",
    // "(Ljava/lang/Object;)Ljava/lang/Object;", false);
    // mv.visitMethodInsn(INVOKESPECIAL, "lisp/special/ReturnThrow", "<init>",
    // "(Llisp/lang/Symbol;Ljava/lang/Object;)V", false);
    // mv.visitInsn(ATHROW);

    // mv.visitLocalVariable("this", "Llisp/special/Return;", null, l0, l1, 0);
    // mv.visitLocalVariable("context", "Llisp/eval/LexicalContext;", null, l0, l1, 1);
    // mv.visitLocalVariable("form", "Ljava/lang/Object;", null, l0, l1, 2);
    // mv.visitMaxs(5, 3);
    // mv.visitEnd();
    // }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final Object expr = expression.get (1);
	context.add (new TypeInsnNode (NEW, "lisp/special/ReturnThrow"));
	context.add (new InsnNode (DUP));
	context.add (new InsnNode (ACONST_NULL)); // Block name
	final CompileResultSet rs = context.compile (expr, true);
	context.convert (rs, Object.class, false, false);
	context.add (new MethodInsnNode (INVOKESPECIAL, "lisp/special/ReturnThrow", "<init>",
	        "(Llisp/lang/Symbol;Ljava/lang/Object;)V", false));
	context.add (new InsnNode (ATHROW));

	return context.compile (expression.last (), true);
    }
}
