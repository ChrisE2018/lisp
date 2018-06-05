
package lisp.cc4;

import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.Symbol;
import lisp.util.LogString;

public class Optimizer extends ClassNode implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Optimizer.class.getName ());

    public Optimizer (final int api, final ClassVisitor classVisitor)
    {
	super (api);
	cv = classVisitor;
    }

    @Override
    public void visitEnd ()
    {
	LOGGER.fine (new LogString ("Optimizer visits class %s %s", name, signature));
	if (Symbol.named ("system", "optimizeJumps").getBooleanValue (true))
	{
	    for (final MethodNode method : methods)
	    {
		removeRedundantJumps (method);
	    }
	}
	accept (cv);
    }

    private void removeRedundantJumps (final MethodNode method)
    {
	System.out.printf ("Method %s %n", method);
	final InsnList il = method.instructions;
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode ins = il.get (i);
	    if (ins instanceof JumpInsnNode)
	    {
		final JumpInsnNode jins = (JumpInsnNode)ins;
		final LabelNode target = jins.label;
		if (il.get (i + 1) == target)
		{
		    il.remove (jins);
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
