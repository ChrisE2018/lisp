
package lisp.asm.instructions;

public class IincInsnNode extends org.objectweb.asm.tree.IincInsnNode
{
    public IincInsnNode (final int var, final int incr)
    {
	super (var, incr);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();

	// buffer.append (getClass ().getSimpleName ());
	// buffer.append (" ");
	buffer.append ("IINC");
	buffer.append (" ");
	buffer.append (var);
	buffer.append (" ");
	if (var > 0)
	{
	    buffer.append ("+");
	}
	buffer.append (var);

	return buffer.toString ();
    }
}
