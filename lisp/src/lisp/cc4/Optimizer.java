
package lisp.cc4;

import java.util.*;
import java.util.logging.*;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.lang.Symbol;
import lisp.util.LogString;

public class Optimizer extends ClassNode implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Optimizer.class.getName ());

    private int removals = 0;

    public Optimizer (final int api, final ClassVisitor classVisitor)
    {
	super (api);
	cv = classVisitor;
    }

    @Override
    public void visitEnd ()
    {
	final int initialCount = countInstructions ();
	LOGGER.finer (new LogString ("Optimizer visits class %s %s with %s instructions", name, signature, initialCount));
	int oldRemovals = removals - 1;
	while (oldRemovals < removals)
	{
	    oldRemovals = removals;
	    if (Symbol.named ("lisp.lang", "optimizeMultipleLabels").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    optimizeMultipleLabels (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "optimizeAfterJump").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    optimizeAfterJump (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "removeStoreLoad").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeStoreLoad (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "optimizeJumpToNext").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeJumpToNext (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "optimizeJumpToJump").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeJumpToJump (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "optimizeDeadLabels").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeDeadLabels (method);
		}
	    }

	    if (Symbol.named ("lisp.lang", "optimizeDoubleReturns").getBooleanValue (true))
	    {
		for (final MethodNode method : methods)
		{
		    removeDoubleReturns (method);
		}
	    }
	    MethodTransformer mt = null;
	    mt = new OptimizeJumpTransformer (mt);
	    mt = new RemoveGetFieldPutFieldTransformer (mt);
	    for (final MethodNode method : methods)
	    {
		mt.transform (method);
	    }
	}
	LOGGER.fine (new LogString ("Optimization of %s removed %s of %s instructions", name, removals, initialCount));
	if (LOGGER.isLoggable (Level.FINEST))
	{
	    for (final MethodNode method : methods)
	    {
		printMethod (method);
	    }
	}
	accept (cv);
    }

    private int countInstructions ()
    {
	int result = 0;
	for (final MethodNode method : methods)
	{
	    result += method.instructions.size ();
	}
	return result;
    }

    /**
     * If two labels appear in a row, change jumps to the second one to jump to the first. Remove
     * dead labels should clear it out.
     */
    private void optimizeMultipleLabels (final MethodNode method)
    {
	LOGGER.finer (new LogString ("optimizeMultipleLabels Method %s (%s so far)", method.name, removals));
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

    /**
     * Remove non-label code right after an unconditional jump
     */
    private void optimizeAfterJump (final MethodNode method)
    {
	LOGGER.finer (new LogString ("optimizeAfterJump Method %s (%s so far)", method.name, removals));
	final InsnList il = method.instructions;
	final List<AbstractInsnNode> remove = new ArrayList<AbstractInsnNode> ();
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof JumpInsnNode)
	    {
		if (ins.getOpcode () == GOTO)
		{
		    final AbstractInsnNode ins2 = il.get (i);
		    if (!(ins2 instanceof LabelNode))
		    {
			remove.add (ins2);
		    }
		}
	    }
	}
	for (final AbstractInsnNode ins : remove)
	{
	    removals++;
	    il.remove (ins);
	}
    }

    /**
     * If STORE n/LOAD n appears in a row, remove the.
     */
    private void removeStoreLoad (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeStoreLoad Method %s (%s so far)", method.name, removals));
	final InsnList il = method.instructions;
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof VarInsnNode)
	    {
		final AbstractInsnNode ins2 = il.get (i);
		if (ins2 instanceof VarInsnNode)
		{
		    final VarInsnNode v1 = (VarInsnNode)ins;
		    final VarInsnNode v2 = (VarInsnNode)ins2;
		    if (v1.var == v2.var)
		    {
			// This opcode must be ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE,
			// FSTORE, DSTORE, ASTORE or RET.
			int matchOp = 0;
			switch (v1.getOpcode ())
			{
			    case ISTORE:
				matchOp = ILOAD;
				break;
			    case LSTORE:
				matchOp = LLOAD;
				break;
			    case FSTORE:
				matchOp = FLOAD;
				break;
			    case DSTORE:
				matchOp = DLOAD;
				break;
			    case ASTORE:
				matchOp = ALOAD;
				break;
			}
			if (v2.getOpcode () == matchOp)
			{
			    il.remove (ins);
			    il.remove (ins2);
			    removals += 2;
			}
		    }
		}
	    }
	}
    }

    /** Remove jumps to a label with a GOTO jump in the next instruction. */
    private void removeJumpToJump (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeJumpToJump Method %s (%s so far)", method.name, removals));
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
	LOGGER.finer (new LogString ("removeJumpToNext Method %s (%s so far)", method.name, removals));
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
			removals++;
		    }
		}
		// If instruction is not a GOTO, change to a POP
		// Implement LOAD/POP deletion
	    }
	}
    }

    /** Remove labels that are not used. */
    private void removeDeadLabels (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeDeadLabels Method %s (%s so far)", method.name, removals));
	final InsnList il = method.instructions;
	final Set<LabelNode> foundLabels = new HashSet<LabelNode> ();
	final Set<LabelNode> usedLabels = new HashSet<LabelNode> ();
	// Ignore references from LocalVariableNodes. They are not really here.
	for (final TryCatchBlockNode handler : method.tryCatchBlocks)
	{
	    usedLabels.add (handler.start);
	    usedLabels.add (handler.end);
	    usedLabels.add (handler.handler);
	}
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
	    removals++;
	}
    }

    /**
     * Change all jump that go to oldLabel to go to newLabel instead. The Opcode of the jump does
     * not matter.
     */
    private void retargetJumps (final InsnList il, final LabelNode oldLabel, final LabelNode newLabel)
    {
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i - 1);
	    if (ins instanceof JumpInsnNode)
	    {
		final JumpInsnNode jins = (JumpInsnNode)ins;

		final LabelNode target = jins.label;
		if (target == oldLabel)
		{
		    jins.label = newLabel;
		    removals++;
		}
	    }
	}
    }

    /** Remove two returns in a row. */
    private void removeDoubleReturns (final MethodNode method)
    {
	LOGGER.finer (new LogString ("removeDoubleReturns Method %s (%s so far)", method.name, removals));
	final InsnList il = method.instructions;
	for (int i = 1; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i);
	    if (isReturnInsn (ins))
	    {
		if (isReturnInsn (il.get (i - 1)))
		{
		    il.remove (ins);
		    removals++;
		}
	    }
	}
    }

    private boolean isReturnInsn (final AbstractInsnNode ins)
    {
	final int op = ins.getOpcode ();
	switch (op)
	{
	    case IRETURN:
	    case LRETURN:
	    case FRETURN:
	    case DRETURN:
	    case ARETURN:
	    case RETURN:
		return true;
	}
	return false;
    }

    private void printMethod (final MethodNode method)
    {
	final InsnList il = method.instructions;
	// Ignore references from LocalVariableNodes. They are not really here.
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i);
	    System.out.printf ("[%d] %s %n", i, ins);
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
	buffer.append (" removed: ");
	buffer.append (removals);
	buffer.append (">");
	return buffer.toString ();
    }
}
