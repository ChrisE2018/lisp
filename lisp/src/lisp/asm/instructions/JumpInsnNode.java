
package lisp.asm.instructions;

import org.objectweb.asm.tree.LabelNode;

public class JumpInsnNode extends org.objectweb.asm.tree.JumpInsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public JumpInsnNode (final InsnCode opcode, final LabelNode label)
    {
	super (opcode.getInstructionCode (), label);
	this.opcode = opcode;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (label);
	return buffer.toString ();
    }
}
