
package lisp.cc4;

import org.objectweb.asm.tree.LabelNode;

public abstract class CompileResult
{
    private final LabelNode l;

    public CompileResult (final LabelNode l)
    {
	this.l = l;
    }

    public LabelNode getLabel ()
    {
	return l;
    }

    public boolean isDefault ()
    {
	return l == null;
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
