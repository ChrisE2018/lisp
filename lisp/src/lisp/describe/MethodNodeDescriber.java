
package lisp.describe;

import java.util.*;

import org.objectweb.asm.tree.*;

import lisp.Describer;
import lisp.asm.instructions.AccessKeywords;

public class MethodNodeDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof MethodNode)
	{
	    final MethodNode mn = (MethodNode)target;
	    return AccessKeywords.find (mn.access) + " method " + mn.name;
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
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final MethodNode mn = (MethodNode)target;
	result.put ("Access", AccessKeywords.find (mn.access));
	result.put ("Name", mn.name);
	result.put ("Description", mn.desc);
	if (mn.signature != null)
	{
	    result.put ("Signature", mn.signature);
	}
	result.put ("Exceptions", mn.exceptions);
	final StringBuilder buffer = new StringBuilder ();
	final List<ParameterNode> parameters = mn.parameters;
	if (parameters != null)
	{
	    for (final ParameterNode p : mn.parameters)
	    {
		if (buffer.length () == 0)
		{
		    buffer.append (", ");
		}
		buffer.append (p.name);
	    }
	    result.put ("Parameters", buffer.toString ());
	}
	result.put ("Instructions", mn.instructions);
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
