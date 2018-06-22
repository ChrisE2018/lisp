
package lisp.cc;

import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.InsnList;

import lisp.cc4.TreeCompilerContext;
import lisp.lang.Symbol;

/**
 * Record of a local variable binding. Subclasses define bindings for local variables and fields.
 *
 * @author cre
 */
abstract public class LexicalBinding
{
    /** The Lisp name of the variable. */
    private final Symbol variable;

    /** The Java class of the variable data. */
    private final Class<?> varClass;

    /** The ASM type of the variable data. */
    private final Type type;

    public LexicalBinding (final Symbol variable, final Class<?> varClass)
    {
	this.variable = variable;
	this.varClass = varClass;
	type = Type.getType (varClass);
    }

    /** The Lisp name of the variable. */
    public Symbol getVariable ()
    {
	return variable;
    }

    /** The Java class of the variable data. */
    public Class<?> getVariableClass ()
    {
	return varClass;
    }

    /** The ASM type of the variable data. */
    public Type getType ()
    {
	return type;
    }

    /**
     * Load value onto the stack by adding instructions.
     *
     * @param il The instructions will be added to the end of this list.
     */
    abstract public void loadValue (final InsnList il);

    /**
     * Load value onto the stack.
     *
     * @param mv
     * @Deprecated Only relevant to compiler V3.
     */
    @Deprecated
    abstract public void loadValue (final GeneratorAdapter mv);

    /**
     * Load value onto the stack.
     *
     * @param context The instructions will be added to the end of the context list.
     */
    abstract public void loadValue (final TreeCompilerContext context);

    /**
     * Store top value from the stack.
     *
     * @param context The instructions will be added to the end of the context list.
     */
    abstract public void store (final TreeCompilerContext context);

    /**
     * Store top value from the stack.
     *
     * @param mv
     * @Deprecated Only relevant to compiler V3.
     */
    @Deprecated
    abstract public void store (final GeneratorAdapter mv);

    /**
     * Increment stored value by one.
     *
     * @param context The instructions will be added to the end of the context list.
     */
    abstract public void increment (final TreeCompilerContext context);

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (variable);
	buffer.append (" ");
	buffer.append (type);
	buffer.append (">");
	return buffer.toString ();
    }
}
