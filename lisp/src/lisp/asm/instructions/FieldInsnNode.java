
package lisp.asm.instructions;

import org.objectweb.asm.Opcodes;

public class FieldInsnNode extends org.objectweb.asm.tree.FieldInsnNode implements Opcodes
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public FieldInsnNode (final InsnCode opcode, final String owner, final String name, final String descriptor)
    {
	super (opcode.getInstructionCode (), owner, name, descriptor);
	this.opcode = opcode;
    }

    public FieldInsnNode (final int opcode, final String owner, final String name, final String descriptor)
    {
	super (opcode, owner, name, descriptor);
	this.opcode = InsnCode.find (opcode);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();

	// buffer.append (getClass ().getSimpleName ());
	// buffer.append (" ");
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (owner);
	buffer.append (" ");
	buffer.append (name);

	return buffer.toString ();
    }
}
