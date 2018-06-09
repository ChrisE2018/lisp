
package lisp.describe;

import org.objectweb.asm.tree.*;

import lisp.Describer;

public class AbstractInsnNodeDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof LabelNode)
	{
	    return target.toString ();
	}
	if (target instanceof AbstractInsnNode)
	{
	    return "      " + target.toString ();
	}
	return target.toString ();
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
