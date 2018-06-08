
package lisp.asm.instructions;

public class LdcInsnNode extends org.objectweb.asm.tree.LdcInsnNode
{
    public LdcInsnNode (final Object value)
    {
	super (value);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("LDC");
	buffer.append (" ");
	buffer.append (cst);
	if (cst != null)
	{
	    buffer.append (" ");
	    buffer.append (cst.getClass ());
	}
	return buffer.toString ();
    }
}
