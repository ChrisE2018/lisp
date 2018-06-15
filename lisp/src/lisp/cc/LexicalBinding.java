
package lisp.cc;

import org.objectweb.asm.Type;

import lisp.Symbol;

/**
 * Record of a local variable binding or for a method parameter.
 *
 * @author cre
 */
public class LexicalBinding
{
    private final Symbol variable;
    private final Class<?> varClass;
    private final int localRef;
    private final Type type;

    public LexicalBinding (final Symbol variable, final Class<?> varClass, final int localRef)
    {
	this.variable = variable;
	this.varClass = varClass;
	this.localRef = localRef;
	type = Type.getType (varClass);
    }

    public Symbol getVariable ()
    {
	return variable;
    }

    public Class<?> getVariableClass ()
    {
	return varClass;
    }

    public int getLocalRef ()
    {
	return localRef;
    }

    public Type getType ()
    {
	return type;
    }

    // abstract public void loadValue (final InsnList il);
    // abstract public void loadValue (final GeneratorAdapter mv);
    // abstract public void loadValue (final TreeCompilerContext context);
    // abstract public void store (final TreeCompilerContext context);
    // abstract public void store (final GeneratorAdapter mv);
    // abstract public void increment (final TreeCompilerContext context);

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
	buffer.append (" ");
	buffer.append (localRef);
	buffer.append (">");
	return buffer.toString ();
    }
}
