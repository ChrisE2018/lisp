
package lisp.asm.instructions;

public class MethodInsnNode extends org.objectweb.asm.tree.MethodInsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public MethodInsnNode (final InsnCode opcode, final String owner, final String name, final String descriptor,
            final boolean isInterface)
    {
	super (opcode.getInstructionCode (), owner, name, descriptor, isInterface);
	this.opcode = opcode;
    }

    public MethodInsnNode (final int opcode, final String owner, final String name, final String descriptor,
            final boolean isInterface)
    {
	super (opcode, owner, name, descriptor, isInterface);
	this.opcode = InsnCode.find (opcode);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (owner);
	buffer.append (" ");
	buffer.append (name);
	buffer.append (" ");
	buffer.append (desc);
	if (itf)
	{
	    buffer.append (" isInterface");
	}
	return buffer.toString ();
    }
}
