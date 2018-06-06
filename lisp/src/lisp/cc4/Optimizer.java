
package lisp.cc4;

import java.util.*;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.Symbol;
import lisp.util.LogString;

public class Optimizer extends ClassNode implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Optimizer.class.getName ());

    private boolean progress = true;

    public Optimizer (final int api, final ClassVisitor classVisitor)
    {
	super (api);
	cv = classVisitor;
    }

    @Override
    public void visitEnd ()
    {
	LOGGER.fine (new LogString ("Optimizer visits class %s %s", name, signature));
	progress = true;
	while (progress)
	{
	    progress = false;
	    if (Symbol.named ("system", "optimizeMultipleLabels").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    optimizeMultipleLabels (method);
		}
	    }

	    if (Symbol.named ("system", "optimizeJumpToJump").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeJumpToJump (method);
		}
	    }

	    if (Symbol.named ("system", "optimizeJumpToNext").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeJumpToNext (method);
		}
	    }

	    if (Symbol.named ("system", "optimizeDeadLabels").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeDeadLabels (method);
		}
	    }
	}
	accept (cv);
    }

    /**
     * If two labels appear in a row, change jumps to the second one to jump to the first. Remove
     * dead labels should clear it out.
     */
    private void optimizeMultipleLabels (final MethodNode method)
    {
	LOGGER.finer (new LogString ("optimizeMultipleLabels Method %s", method.name));
	final InsnList il = method.instructions;
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof LabelNode)
	    {
		final AbstractInsnNode ins2 = il.get (i);
		if (ins2 instanceof LabelNode)
		{
		    retargetJumps (il, (LabelNode)ins2, (LabelNode)ins);
		}
	    }
	}
    }

    /** Remove jumps to a label with a GOTO jump in the next instruction. */
    private void removeJumpToJump (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeJumpToJump Method %s", method.name));
	final InsnList il = method.instructions;
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ln = il.get (i - 1);
	    if (ln instanceof LabelNode)
	    {
		final LabelNode oldLabel = (LabelNode)ln;
		final AbstractInsnNode ins = il.get (i);
		if (ins instanceof JumpInsnNode)
		{
		    final JumpInsnNode jins = (JumpInsnNode)ins;
		    if (ins.getOpcode () == GOTO)
		    {
			// We have a label followed by a GOTO
			final LabelNode target = jins.label;
			retargetJumps (il, oldLabel, target);
		    }
		}
	    }
	}
    }

    /** Remove jumps to a label in the next instruction. */
    private void removeJumpToNext (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeJumpToNext Method %s", method.name));
	final InsnList il = method.instructions;
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof JumpInsnNode)
	    {
		final JumpInsnNode jins = (JumpInsnNode)ins;
		if (ins.getOpcode () == GOTO)
		{
		    final LabelNode target = jins.label;
		    if (il.get (i) == target)
		    {
			il.remove (jins);
			progress = true;
		    }
		}
	    }
	}
    }

    /** Remove labels that are not used. */
    private void removeDeadLabels (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeDeadLabels Method %s", method.name));
	final InsnList il = method.instructions;
	final Set<LabelNode> foundLabels = new HashSet<LabelNode> ();
	final Set<LabelNode> usedLabels = new HashSet<LabelNode> ();
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i);
	    if (ins instanceof JumpInsnNode)
	    {
		final JumpInsnNode jins = (JumpInsnNode)ins;
		final LabelNode target = jins.label;
		usedLabels.add (target);
	    }
	    if (ins instanceof LabelNode)
	    {
		foundLabels.add ((LabelNode)ins);
	    }
	}
	foundLabels.removeAll (usedLabels);
	for (final LabelNode l : foundLabels)
	{
	    il.remove (l);
	    progress = true;
	}
    }

    /**
     * Change all jump that go to oldLabel to go to newLabel instead. The Opcode of the jump does
     * not matter.
     */
    private void retargetJumps (final InsnList il, final LabelNode oldLabel, final LabelNode newLabel)
    {
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof JumpInsnNode)
	    {
		final JumpInsnNode jins = (JumpInsnNode)ins;

		final LabelNode target = jins.label;
		if (target == oldLabel)
		{
		    jins.label = newLabel;
		    progress = true;
		}
	    }
	}
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
