
package lisp.cc;

import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.eval.*;

public class SpecialForms extends Definer
{
    // [TODO] Need a way to define special form compilers here
    // That requires access to the CompileClassAdaptor methods and state.
    interface CompilerGuts
    {
	void pushDefaultValue (GeneratorAdapter mv, Class<?> valueType);

	void compileExpression (GeneratorAdapter mv, Object expression, final Class<?> valueType);
    }

    @DefineLisp (compiler = true)
    public void compileProgn (final CompilerGuts guts, final GeneratorAdapter mv, final LispList expr, final Class<?> valueType)
    {
	// (define foo () (progn (printf "a%n") (printf "b%n") 3))
	if (expr.size () == 0)
	{
	    guts.pushDefaultValue (mv, valueType);
	}
	else
	{
	    for (int i = 1; i < expr.size () - 1; i++)
	    {
		guts.compileExpression (mv, expr.get (i), null);
	    }
	    guts.compileExpression (mv, expr.last (), valueType);
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
