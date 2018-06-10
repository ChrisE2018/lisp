
package lisp.cc4;

import org.objectweb.asm.tree.ClassNode;

public class ClassTransformer
{
    protected ClassTransformer ct;

    public ClassTransformer (final ClassTransformer ct)
    {
	this.ct = ct;
    }

    public void transform (final ClassNode cn)
    {
	if (ct != null)
	{
	    ct.transform (cn);
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
