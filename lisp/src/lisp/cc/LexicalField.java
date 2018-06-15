
package lisp.cc;

import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.InsnList;

import lisp.Symbol;
import lisp.cc4.TreeCompilerContext;

public class LexicalField extends LexicalBinding
{
    public LexicalField (final Symbol variable, final Class<?> varClass)
    {
	super (variable, varClass);
    }

    @Override
    public void loadValue (final InsnList il)
    {
	throw new Error ("NYI");
    }

    @Override
    public void loadValue (final GeneratorAdapter mv)
    {
	throw new Error ("NYI");
    }

    @Override
    public void loadValue (final TreeCompilerContext context)
    {
	throw new Error ("NYI");
    }

    @Override
    public void store (final TreeCompilerContext context)
    {
	throw new Error ("NYI");
    }

    @Override
    public void store (final GeneratorAdapter mv)
    {
	throw new Error ("NYI");
    }

    @Override
    public void increment (final TreeCompilerContext context)
    {
	throw new Error ("NYI");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getVariable ());
	buffer.append (" ");
	buffer.append (getType ());
	buffer.append (">");
	return buffer.toString ();
    }
}
