
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class RepeatFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);

	visitor.visitInteger (expression.get (1));
	for (int i = 2; i < expression.size () - 1; i++)
	{
	    visitor.visitIgnored (expression.get (i));
	}
	visitor.visitValue (expression.last ());
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo () (repeat 10 (printf "foo")))
	// (define foo () (repeat 3 (printf "foo")))

	final LabelNode l0 = new LabelNode ();
	final LabelNode l1 = new LabelNode ();

	final CompileResultSet repeatCount = context.compile (expression.get (1), true);
	context.convert (repeatCount, int.class, false, false);
	context.add (new InsnNode (ICONST_0));
	context.add (l1);
	context.add (new InsnNode (DUP2));
	context.add (new JumpInsnNode (IF_ICMPLE, l0));

	for (int i = 2; i < expression.size (); i++)
	{
	    final CompileResultSet r = context.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    context.convert (r, void.class, false, false);
	}
	context.add (new InsnNode (ICONST_1));
	context.add (new InsnNode (IADD));
	context.add (new JumpInsnNode (GOTO, l1));

	context.add (l0);
	context.add (new InsnNode (POP2));
	// Always return false
	final LabelNode ll = new LabelNode ();
	context.add (new JumpInsnNode (GOTO, ll));
	return new CompileResultSet (new ImplicitCompileResult (ll, false));
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (repeat x 3))
	// (define foo (x) (repeat x 3) 5)
	// (define foo (x) (printf "bar%n"))
	// (define foo (x) (repeat x (printf "bar%n")))
	// (define foo (x) (repeat x (printf "bar%n")) 5)
	// (define foo (x) (repeat x (not true)))
	// (define foo (x) (repeat x true))
	// (define foo (x) (repeat x (abs 5)))
	// (define foo (x) (repeat x (printf "bar %s%n" x)))
	// (define foo (x) (repeat x (setq a (+ a 1))))
	// (define foo (x) (repeat (+ x 5) (setq a (+ a 1))))
	// (define foo (x y) (repeat (+ x 5) (setq a (+ a y))))
	// (define foo (x) (repeat x (printf "foo")) 5)
	// (define int:foo () (repeat 1000 3))

	// Compute repeat count
	generator.compileExpression (mv, e.get (1), int.class, false, false);
	// Make a local variable for repeat count
	final int countRef = mv.newLocal (Type.getType (int.class));
	mv.visitVarInsn (ISTORE, countRef);

	// Push default return value onto the stack
	// generator.pushDefaultValue (mv, valueType, false);

	// Push iteration number onto the stack
	mv.visitInsn (ICONST_0);

	// Jump to termination test
	final Label l1 = new Label ();
	mv.visitJumpInsn (GOTO, l1);

	// Start of iteration body
	final Label l2 = new Label ();
	mv.visitLabel (l2);
	// Stack: iteration

	// <body code goes here>
	// Stack: iteration
	for (int i = 2; i < e.size (); i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}

	// Loop increment
	// Stack: iteration
	mv.visitInsn (ICONST_1);
	mv.visitInsn (IADD);

	// Termination test
	// Stack: iteration
	mv.visitLabel (l1);
	mv.visitInsn (DUP);
	mv.visitVarInsn (ILOAD, countRef);
	// Stack: count, iteration, iteration
	mv.visitJumpInsn (IF_ICMPLT, l2);
	// Stack: iteration

	// Return final value
	// Remove iteration count
	mv.visitInsn (POP);
	// Always return false
	mv.visitInsn (ICONST_0);
	generator.convert (mv, boolean.class, valueType, allowNarrowing, liberalTruth);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
