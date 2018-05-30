
package lisp.special;

import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.cc.CompilerGenerator;
import lisp.eval.*;

public class Progn extends Definer
{
    /**
     * Interpreter for progn forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     */
    @DefineLisp (special = true)
    public Object progn (final LexicalContext context, final Object... arguments) throws Exception
    {
	Object result = null;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    result = context.eval (arg);
	}
	return result;
    }

    @DefineLisp (special = true, name = "progn", compiler = true)
    public void compileProgn (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expr,
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
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
