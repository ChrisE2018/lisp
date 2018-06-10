
package lisp.cc4;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.MethodNode;

public class MethodTransformer implements Opcodes
{
    protected MethodTransformer mt;

    public MethodTransformer (final MethodTransformer mt)
    {
	this.mt = mt;
    }

    public void transform (final MethodNode mn)
    {
	if (mt != null)
	{
	    mt.transform (mn);
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
