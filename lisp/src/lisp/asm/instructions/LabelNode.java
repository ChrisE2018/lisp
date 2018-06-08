
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
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (getLabel ());
	return buffer.toString ();
    }
}
