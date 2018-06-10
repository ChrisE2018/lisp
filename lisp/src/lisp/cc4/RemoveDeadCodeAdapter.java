
package lisp.cc4;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.analysis.*;
import org.objectweb.asm.tree.analysis.Frame;

// From ASM documentation
public class RemoveDeadCodeAdapter extends MethodVisitor implements Opcodes
{
    String owner;
    MethodVisitor next;

    public RemoveDeadCodeAdapter (final String owner, final int access, final String name, final String desc,
            final MethodVisitor mv)
    {
	super (ASM5, new MethodNode (access, name, desc, null, null));
	this.owner = owner;
	next = mv;
    }

    @Override
    public void visitEnd ()
    {
	final MethodNode mn = (MethodNode)mv;
	final Analyzer<BasicValue> a = new Analyzer<BasicValue> (new BasicInterpreter ());
	try
	{
	    a.analyze (owner, mn);
	    final Frame<BasicValue>[] frames = a.getFrames ();
	    final AbstractInsnNode[] insns = mn.instructions.toArray ();
	    for (int i = 0; i < frames.length; ++i)
	    {
		if (frames[i] == null && !(insns[i] instanceof LabelNode))
		{
		    mn.instructions.remove (insns[i]);
		}
	    }
	}
	catch (final AnalyzerException ignored)
	{
	}
	mn.accept (next);
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
