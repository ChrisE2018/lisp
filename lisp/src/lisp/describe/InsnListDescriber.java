
package lisp.describe;

import org.objectweb.asm.tree.*;

import lisp.Describer;
import lisp.util.MultiMap;

public class InsnListDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof InsnList)
	{
	    return ((InsnList)target).size () + " instructions";
	}
	else
	{
	    return target.toString ();
	}
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	final InsnList il = (InsnList)target;
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode instruction = il.get (i);
	    result.put (String.valueOf (i), instruction);
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
