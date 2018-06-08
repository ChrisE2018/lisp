
package lisp.asm.instructions;

import org.objectweb.asm.tree.LabelNode;

public class LookupSwitchInsnNode extends org.objectweb.asm.tree.LookupSwitchInsnNode
{
    public LookupSwitchInsnNode (final LabelNode dflt, final int[] keys, final LabelNode[] labels)
    {
	super (dflt, keys, labels);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (dflt);
	buffer.append (" ");
	buffer.append (keys);
	buffer.append (" ");
	buffer.append (labels);
	return buffer.toString ();
    }
}
