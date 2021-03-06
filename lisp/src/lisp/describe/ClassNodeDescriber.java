
package lisp.describe;

import java.util.List;

import org.objectweb.asm.tree.*;

import lisp.asm.instructions.AccessKeywords;
import lisp.lang.Describer;
import lisp.util.MultiMap;

public class ClassNodeDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	final ClassNode cn = (ClassNode)target;
	result.put ("Version", cn.version);
	result.put ("Access", AccessKeywords.find (cn.access));
	result.put ("Name", cn.name);
	result.put ("Signature", cn.signature);
	for (final FieldNode field : cn.fields)
	{
	    result.put ("Field", field);
	}
	final List<MethodNode> methods = cn.methods;
	for (final MethodNode method : methods)
	{
	    result.put ("Method", method);
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
