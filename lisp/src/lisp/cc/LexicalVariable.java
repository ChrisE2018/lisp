
package lisp.cc;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.InsnList;

import lisp.Symbol;
import lisp.asm.instructions.*;
import lisp.cc4.TreeCompilerContext;

public class LexicalVariable extends LexicalBinding
{
    private final int localRef;

    public LexicalVariable (final Symbol variable, final Class<?> varClass, final int localRef)
    {
	super (variable, varClass);
	this.localRef = localRef;
    }

    // @Override
    public int getLocalRef ()
    {
	return localRef;
    }

    @Override
    public void loadValue (final InsnList il)
    {
	il.add (new VarInsnNode (getType ().getOpcode (Opcodes.ILOAD), localRef));
    }

    @Override
    public void loadValue (final GeneratorAdapter mv)
    {
	mv.loadLocal (localRef);
    }

    @Override
    public void loadValue (final TreeCompilerContext context)
    {
	context.add (new VarInsnNode (getType ().getOpcode (Opcodes.ILOAD), localRef));
    }

    @Override
    public void store (final TreeCompilerContext context)
    {
	context.add (new VarInsnNode (getType ().getOpcode (Opcodes.ISTORE), localRef));
    }

    @Override
    public void store (final GeneratorAdapter mv)
    {
	mv.storeLocal (localRef);
    }

    @Override
    public void increment (final TreeCompilerContext context)
    {
	context.add (new IincInsnNode (localRef, 1));
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
	buffer.append (" ");
	buffer.append (localRef);
	buffer.append (">");
	return buffer.toString ();
    }
}
