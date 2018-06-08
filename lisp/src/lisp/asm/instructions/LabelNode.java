
package lisp.asm.instructions;

public class LabelNode extends org.objectweb.asm.tree.LabelNode
{
    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (getLabel ());
	return buffer.toString ();
    }
}
