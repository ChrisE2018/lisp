
package lisp.cc4;

import java.util.Iterator;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.*;

/** From ASM documentation. */
public class OptimizeJumpTransformer extends MethodTransformer implements Opcodes
{
    public OptimizeJumpTransformer (final MethodTransformer mt)
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
	    final AbstractInsnNode in = i.next ();
	    if (in instanceof JumpInsnNode)
	    {
		LabelNode label = ((JumpInsnNode)in).label;
		AbstractInsnNode target;
		// while target == goto l, replace label with l
		while (true)
		{
		    target = label;
		    while (target != null && target.getOpcode () < 0)
		    {
			target = target.getNext ();
		    }
		    if (target != null && target.getOpcode () == GOTO)
		    {
			label = ((JumpInsnNode)target).label;
		    }
		    else
		    {
			break;
		    }
		}
		// update target
		((JumpInsnNode)in).label = label;
		// if possible, replace jump with target instruction
		if (in.getOpcode () == GOTO && target != null)
		{
		    final int op = target.getOpcode ();
		    if ((op >= IRETURN && op <= RETURN) || op == ATHROW)
		    {
			// replace ’in’ with clone of ’target’
			insns.set (in, target.clone (null));
		    }
		}
	    }
	}
	super.transform (mn);
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
