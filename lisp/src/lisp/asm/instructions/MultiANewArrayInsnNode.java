
package lisp.asm.instructions;

public class MultiANewArrayInsnNode extends org.objectweb.asm.tree.MultiANewArrayInsnNode
{
    public MultiANewArrayInsnNode (final String descriptor, final int numDimensions)
    {
	super (descriptor, numDimensions);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (desc);
	buffer.append (" ");
	buffer.append (dims);
	return buffer.toString ();
    }
}
