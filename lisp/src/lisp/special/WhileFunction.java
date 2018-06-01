
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.CompilerGenerator;
import lisp.symbol.LispFunction;

public class WhileFunction extends LispFunction implements Opcodes
{
    public WhileFunction (final Symbol symbol)
    {
	super (symbol);
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
	buffer.append (getSymbol ());
	buffer.append (">");
	return buffer.toString ();
    }
}
