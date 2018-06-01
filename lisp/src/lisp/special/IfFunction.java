
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.CompilerGenerator;
import lisp.symbol.*;

public class IfFunction extends LispFunction implements Opcodes
{
    public IfFunction (final Symbol symbol)
    {
	super (symbol);
    }

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
	buffer.append (getSymbol ());
	buffer.append (">");
	return buffer.toString ();
    }
}
