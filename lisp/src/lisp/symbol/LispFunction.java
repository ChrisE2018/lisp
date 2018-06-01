
package lisp.symbol;

import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.cc.CompilerGenerator;

/**
 * Base class for functions to support a Lisp function. This includes compiler and walker methods.
 *
 * @author cre
 */
abstract public class LispFunction
{
    private final Symbol symbol;

    public LispFunction (final Symbol symbol)
    {
	this.symbol = symbol;
    }

    abstract public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth);

    public Symbol getSymbol ()
    {
	return symbol;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (symbol);
	buffer.append (">");
	return buffer.toString ();
    }
}
