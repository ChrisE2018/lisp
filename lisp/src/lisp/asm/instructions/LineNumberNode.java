
package lisp.asm.instructions;

import org.objectweb.asm.tree.LabelNode;

public class LineNumberNode extends org.objectweb.asm.tree.LineNumberNode
{

    public LineNumberNode (final int line, final LabelNode start)
    {
	super (line, start);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("Line: ");
	buffer.append (line);
	buffer.append (" ");
	buffer.append (start);
	return buffer.toString ();
    }
}
