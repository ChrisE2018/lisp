
package lisp.asm;

import java.util.Iterator;

import org.objectweb.asm.tree.*;

/** From ASM documentation. */
public class RemoveGetFieldPutFieldTransformer extends MethodTransformer
{
    public RemoveGetFieldPutFieldTransformer (final MethodTransformer mt)
    {
	super (mt);
    }

    @Override
    public void transform (final MethodNode mn)
    {
	final InsnList insns = mn.instructions;
	final Iterator<AbstractInsnNode> i = insns.iterator ();
	while (i.hasNext ())
	{
	    final AbstractInsnNode i1 = i.next ();
	    if (isALOAD0 (i1))
	    {
		final AbstractInsnNode i2 = getNext (i1);
		if (i2 != null && isALOAD0 (i2))
		{
		    final AbstractInsnNode i3 = getNext (i2);
		    if (i3 != null && i3.getOpcode () == GETFIELD)
		    {
			final AbstractInsnNode i4 = getNext (i3);
			if (i4 != null && i4.getOpcode () == PUTFIELD)
			{
			    if (sameField (i3, i4))
			    {
				while (i.next () != i4)
				{
				}
				insns.remove (i1);
				insns.remove (i2);
				insns.remove (i3);
				insns.remove (i4);
			    }
			}
		    }
		}
	    }
	}
	super.transform (mn);
    }

    private static AbstractInsnNode getNext (final AbstractInsnNode in)
    {
	AbstractInsnNode insn = in;
	do
	{
	    insn = insn.getNext ();
	    if (insn != null && !(insn instanceof LineNumberNode))
	    {
		break;
	    }
	}
	while (insn != null);
	return insn;
    }

    private static boolean isALOAD0 (final AbstractInsnNode i)
    {
	return i.getOpcode () == ALOAD && ((VarInsnNode)i).var == 0;
    }

    private static boolean sameField (final AbstractInsnNode i, final AbstractInsnNode j)
    {
	return ((FieldInsnNode)i).name.equals (((FieldInsnNode)j).name);
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
