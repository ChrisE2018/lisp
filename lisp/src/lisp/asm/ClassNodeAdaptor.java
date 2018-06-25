
package lisp.asm;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.tree.ClassNode;

public class ClassNodeAdaptor extends ClassNode
{
    public ClassNodeAdaptor (final int type, final ClassVisitor cv)
    {
	super (type);
	this.cv = cv;
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
