
package lisp.cc;

import org.objectweb.asm.Type;

import lisp.Symbol;

public class LocalBinding
{
    private final Symbol variable;
    private final Type type;
    private final int localRef;

    public LocalBinding (final Symbol variable, final Type type, final int localRef)
    {
	this.variable = variable;
	this.type = type;
	this.localRef = localRef;
    }

    public Symbol getVariable ()
    {
	return variable;
    }

    public Type getType ()
    {
	return type;
    }

    public int getLocalRef ()
    {
	return localRef;
    }

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