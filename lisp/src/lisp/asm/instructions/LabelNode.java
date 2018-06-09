
package lisp.asm.instructions;

import org.objectweb.asm.Label;

public class LabelNode extends org.objectweb.asm.tree.LabelNode
{
    public LabelNode (final Label l1)
    {
	super (l1);
    }

    public LabelNode ()
    {
	super ();
    }

    @Override
    public String toString ()
    {
	/**
	 * This does not have access to the analyzed and optimized bytecode. Should run the final
	 * bytecode though ASM again to make a new ClassNode containing the optimized bytecode and
	 * then convert the instructions into lisp.asm versions while retaining the original ASM
	 * labels.
	 */
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("L");
	final Label label = getLabel ();
	if (label != null)
	{
	    int offset = 0;
	    try
	    {
		offset = label.getOffset ();
	    }
	    catch (final IllegalStateException e)
	    {
	    }
	    if (offset > 0)
	    {
		buffer.append (offset);
	    }
	    else
	    {
		buffer.append (System.identityHashCode (label));
	    }
	}
	else
	{
	    buffer.append (System.identityHashCode (this));
	}
	// buffer.append (" ");
	// buffer.append (GetPackageInternals.getLabelInputStackSize (label));
	return buffer.toString ();
    }
}
