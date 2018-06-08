
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
