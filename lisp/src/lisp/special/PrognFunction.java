
package lisp.special;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.cc.CompilerGenerator;
import lisp.symbol.LispFunction;

public class PrognFunction extends LispFunction implements Opcodes
{
    public PrognFunction (final Symbol symbol)
    {
	super (symbol);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expr,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo () (progn (printf "a%n") (printf "b%n") 3))
	if (expr.size () == 0)
	{
	    generator.pushDefaultValue (mv, valueType, true);
	}
	else
	{
	    for (int i = 1; i < expr.size () - 1; i++)
	    {
		generator.compileExpression (mv, expr.get (i), null, false, false);
	    }
	    generator.compileExpression (mv, expr.last (), valueType, allowNarrowing, liberalTruth);
	}
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
