
package lisp.asm.instructions;

public class VarInsnNode extends org.objectweb.asm.tree.VarInsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public VarInsnNode (final InsnCode opcode, final int var)
    {
	super (opcode.getInstructionCode (), var);
	this.opcode = opcode;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (var);
	return buffer.toString ();
    }
}
