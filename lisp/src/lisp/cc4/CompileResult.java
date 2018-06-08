
package lisp.cc4;

import java.util.*;

import org.objectweb.asm.tree.LabelNode;

public abstract class CompileResult
{
    private final List<LabelNode> labels;

    /**
     * @return
     * @Deprecated Always insert a goto a let optimizer deal with it.
     */
    @Deprecated
    public CompileResult ()
    {
	labels = new ArrayList<LabelNode> ();
    }

    public CompileResult (final LabelNode label)
    {
	labels = new ArrayList<LabelNode> ();
	labels.add (label);
	if (label == null)
	{
	    throw new Error ("yiuck");
	}
    }

    public CompileResult (final List<LabelNode> labels)
    {
	this.labels = labels;
    }

    public List<LabelNode> getLabels ()
    {
	return labels;
    }

    public void addLabels (final List<LabelNode> more)
    {
	if (labels.isEmpty ())
	{
	    throw new Error ("Compiler error, can't convert default result to jump");
	}
	labels.addAll (more);
    }

    /**
     * @return
     * @Deprecated Always insert a goto a let optimizer deal with it.
     */
    @Deprecated
    public boolean isDefault ()
    {
	return labels.isEmpty ();
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
