
package lisp.asm.instructions;

public class InsnNode extends org.objectweb.asm.tree.InsnNode
{
    @SuppressWarnings ("hiding")
    final InsnCode opcode;

    public InsnNode (final InsnCode opcode)
    {
	super (opcode.getInstructionCode ());
	this.opcode = opcode;
    }

    public InsnNode (final int opcode)
    {
	super (opcode);
	this.opcode = InsnCode.find (opcode);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	// buffer.append ("#<");
	// buffer.append (getClass ().getSimpleName ());
	buffer.append (opcode);
	// buffer.append (" ");
	// buffer.append (System.identityHashCode (this));
	// buffer.append (">");
	return buffer.toString ();
    }
}
