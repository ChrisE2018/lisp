
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

    /** Call visitor on all directly nested subexpressions. */
    abstract public void walker (LispVisitor visitor, final LispList expression);

    /** Compile to bytecode. */
    abstract public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth);

    /** Special case compiler when return value is not required. */
    public void compile2void (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression)
    {
	compile (generator, mv, expression, null, false, false);
    }

    /** Special case compiler when return value must be an integer. */
    public void compile2int (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression)
    {
	compile (generator, mv, expression, int.class, false, false);
    }

    /**
     * Special case compiler when return value must be an double. Only values that can be assigned
     * to double are accepted. This includes int, Integer, short, Short, float, Float, double and
     * Double.
     */
    public void compile2double (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression)
    {
	compile (generator, mv, expression, double.class, false, false);
    }

    /**
     * Special case compiler when return value must be a boolean. Only values that are strictly
     * boolean (or Boolean) are accepted.
     */
    public void compile2boolean (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression)
    {
	compile (generator, mv, expression, boolean.class, false, false);
    }

    /**
     * Special case compiler when return value must be a boolean. Anything except boolean false is
     * accepted as true.
     */
    public void compile2truth (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression)
    {
	compile (generator, mv, expression, boolean.class, false, true);
    }

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
