
package lisp.asm.instructions;

public class IntInsnNode extends org.objectweb.asm.tree.IntInsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public IntInsnNode (final InsnCode opcode, final int operand)
    {
	super (opcode.getInstructionCode (), operand);
	this.opcode = opcode;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (operand);
	return buffer.toString ();
    }
}
