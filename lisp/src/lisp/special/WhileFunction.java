
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.*;

import lisp.LispList;
import lisp.cc.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class WhileFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	visitor.visitBoolean (expression.get (1));
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
	// (define foo () (while true (printf "foo")))

	final LabelNode l0 = new LabelNode ();
	final LabelNode l1 = new LabelNode ();

	context.add (l1);
	final CompileResultSet testResultSet = context.compile (expression.get (1), true);
	context.convertIfFalse (testResultSet, false, true, l0);

	for (int i = 2; i < expression.size (); i++)
	{
	    final CompileResultSet r = context.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    context.convert (r, void.class, false, false);
	}
	context.add (new JumpInsnNode (GOTO, l1));

	context.add (l0);
	// Always return false
	return new CompileResultSet (new ImplicitCompileResult (null, false));
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (setq a 0) (while (< a x) (printf "A: %s%n" a) (setq a (+ a 1))))

	// Load default value
	generator.pushDefaultValue (mv, valueType, false);

	// Perform iteration test
	final Label l1 = new Label ();
	mv.visitLabel (l1);
	final Label l2 = new Label ();
	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	mv.visitJumpInsn (IFEQ, l2);

	// Loop body
	if (valueType != null)
	{
	    mv.visitInsn (POP);
	}
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Don't pop the last value
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	mv.visitJumpInsn (GOTO, l1);

	mv.visitLabel (l2);
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
