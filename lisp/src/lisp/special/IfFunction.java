
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class IfFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	visitor.visitBoolean (expression.get (1));
	visitor.visitValue (expression.get (2));
	if (expression.size () > 3)
	{
	    for (int i = 3; i < expression.size () - 1; i++)
	    {
		visitor.visitIgnored (expression.get (i));
	    }
	    visitor.visitValue (expression.last ());
	}
	else
	{
	    // May return a default value
	}
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	if (expression.size () == 3)
	{
	    // (define foo (boolean:x) (if x 3))
	    final CompileResultSet testResultSet = context.compile (expression.get (1), true);
	    final LabelNode lFalse = new LabelNode ();// This label means we return false
	    context.convertIfFalse (testResultSet, false, true, lFalse);
	    final CompileResultSet result = context.compile (expression.last (), true);
	    result.addImplicitCompileResult (lFalse, false);
	    return result;
	}
	else
	{
	    // (define foo (boolean:x) (if x 3 4))
	    // (define foo () (if 'bar 3 4))
	    final CompileResultSet testResultSet = context.compile (expression.get (1), true);
	    final LabelNode lFalse = new LabelNode ();// This label means we return false
	    context.convertIfFalse (testResultSet, false, true, lFalse);

	    final CompileResultSet result = context.compile (expression.get (2), true);

	    // false case
	    context.add (lFalse);
	    context.add (new LineNumberNode (65, lFalse));
	    for (int i = 3; i < expression.size () - 1; i++)
	    {
		final CompileResultSet r = context.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		context.convert (r, void.class, false, false);
	    }
	    final CompileResultSet fresult = context.compile (expression.last (), true);
	    for (final CompileResult cr : fresult.getResults ())
	    {
		result.add (cr);
	    }
	    return result;
	}
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (expression.size () <= 3)
	{
	    // No else clause, compile as when
	    compileOneCaseIf (generator, mv, expression, valueType, allowNarrowing, liberalTruth);
	}
	else
	{
	    // If then else
	    compileTwoCaseIf (generator, mv, expression, valueType, allowNarrowing, liberalTruth);
	}
    }

    public void compileOneCaseIf (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (when x 1 2))
	// (define foo (x) (when x 1 (printf "a%n") (printf "b%n") 3))

	final Label l1 = new Label ();
	final Label l2 = new Label ();
	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	mv.visitJumpInsn (IFEQ, l2);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Don't pop the last value
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	mv.visitJumpInsn (GOTO, l1);

	// False case.
	mv.visitLabel (l2);
	generator.pushDefaultValue (mv, valueType, false);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l1);
    }

    private void compileTwoCaseIf (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (if x 1 2))
	// (define foo (x) (if x 1 (printf "a%n") (printf "b%n") 3))

	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	mv.visitJumpInsn (IFEQ, l2);

	// True case
	generator.compileExpression (mv, e.get (2), valueType, allowNarrowing, liberalTruth);
	mv.visitJumpInsn (GOTO, l1);

	// False case. Nothing on the stack.
	mv.visitLabel (l2);
	for (int i = 3; i < e.size () - 1; i++)
	{
	    // valueType null means nothing is left on the stack
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	if (e.size () <= 3)
	{
	    // No false case so return default
	    generator.pushDefaultValue (mv, valueType, false);
	}
	else
	{
	    // Return the last value
	    generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	}

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l1);
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
