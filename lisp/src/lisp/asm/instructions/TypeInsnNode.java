
package lisp.asm.instructions;

public class TypeInsnNode extends org.objectweb.asm.tree.TypeInsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public TypeInsnNode (final InsnCode opcode, final String descriptor)
    {
	super (opcode.getInstructionCode (), descriptor);
	this.opcode = opcode;
    }

    public TypeInsnNode (final int opcode, final String descriptor)
    {
	super (opcode, descriptor);
	this.opcode = InsnCode.find (opcode);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (opcode);
	buffer.append (" ");
	buffer.append (desc);
	return buffer.toString ();
    }
}
