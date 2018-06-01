
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.CompilerGenerator;
import lisp.symbol.LispFunction;

public class UnlessFunction extends LispFunction implements Opcodes
{
    public UnlessFunction (final Symbol symbol)
    {
	super (symbol);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (unless x 1 2))
	// (define foo (x) (unless x 1 (printf "a%n") (printf "b%n") 3))
	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Don't pop the last value
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	final Label l3 = new Label ();
	mv.visitJumpInsn (GOTO, l3);

	// False case where we must pop a value
	mv.visitLabel (l1);
	generator.pushDefaultValue (mv, valueType, true);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l3);
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
