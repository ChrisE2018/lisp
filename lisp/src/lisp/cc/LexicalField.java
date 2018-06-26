
package lisp.cc;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.InsnList;

import lisp.asm.instructions.*;
import lisp.cc4.TreeCompilerContext;
import lisp.lang.Symbol;

public class LexicalField extends LexicalBinding
{
    private final Type classType;
    private final boolean isStatic;

    public LexicalField (final Symbol variable, final boolean isStatic, final Class<?> varClass, final Type classType)
    {
	super (variable, varClass);
	this.isStatic = isStatic;
	this.classType = classType;
    }

    @Override
    public void loadValue (final InsnList il)
    {
	if (isStatic)
	{
	    final String desc = getType ().getDescriptor ();
	    il.add (new FieldInsnNode (Opcodes.GETSTATIC, classType.getInternalName (), getVariable ().getName (), desc));
	}
	else
	{
	    il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	    final String desc = getType ().getDescriptor ();
	    il.add (new FieldInsnNode (Opcodes.GETFIELD, classType.getInternalName (), getVariable ().getName (), desc));
	}
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
	if (isStatic)
	{
	    final String desc = getType ().getDescriptor ();
	    context.add (new FieldInsnNode (Opcodes.PUTSTATIC, classType.getInternalName (), getVariable ().getName (), desc));
	}
	else
	{
	    context.add (new VarInsnNode (Opcodes.ALOAD, 0));
	    context.add (new InsnNode (Opcodes.SWAP));
	    final String desc = getType ().getDescriptor ();
	    context.add (new FieldInsnNode (Opcodes.PUTFIELD, classType.getInternalName (), getVariable ().getName (), desc));
	}
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
