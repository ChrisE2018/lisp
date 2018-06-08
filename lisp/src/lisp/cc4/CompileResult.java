
package lisp.cc4;

import java.util.*;

import org.objectweb.asm.tree.LabelNode;

public abstract class CompileResult
{
    private final List<org.objectweb.asm.tree.LabelNode> labels;

    public CompileResult (final org.objectweb.asm.tree.LabelNode label)
    {
	labels = new ArrayList<org.objectweb.asm.tree.LabelNode> ();
	labels.add (label);
	if (label == null)
	{
	    throw new Error ("yiuck");
	}
    }

    public CompileResult (final List<org.objectweb.asm.tree.LabelNode> labels)
    {
	this.labels = labels;
    }

    public List<org.objectweb.asm.tree.LabelNode> getLabels ()
    {
	return labels;
    }

    public void addLabels (final List<? extends org.objectweb.asm.tree.LabelNode> more)
    {
	if (labels.isEmpty ())
	{
	    throw new Error ("Compiler error, can't convert default result to jump");
	}
	labels.addAll (more);
    }

    abstract public Class<?> getResultClass ();

    abstract public CompileResult getJumpTo (LabelNode ll);

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
