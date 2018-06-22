
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;
import lisp.lang.Symbol;

public class Setq extends Definer implements Opcodes
{
    /**
     * Interpreter for setq statements.
     *
     * @param interpreter The interpreter used to evaluate forms.
     * @param arguments The symbol and value form.
     * @return The new value.
     */
    @DefineLisp (special = true, classname = "lisp.special.SetqFunction")
    public Object setq (final LexicalContext context, final Symbol symbol, final Object expr) throws Exception
    {
	final Object value = context.eval (expr);
	context.set (symbol, value);
	return value;
    }

    @DefineLisp
    public Object symbolValue (final Symbol arg)
    {
	return arg.getValue ();
    }

    /**
     * Set an evaluated symbol value. This is intended for use in macros that produce a form which
     * will be evaluated to determine the symbol to modify.
     */
    @DefineLisp (name = "setSymbolValue")
    public Object setSymbolValue (final Symbol symbol, final Object value)
    {
	symbol.setValue (value);
	return value;
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
