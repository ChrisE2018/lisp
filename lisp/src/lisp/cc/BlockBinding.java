
package lisp.cc;

import org.objectweb.asm.tree.LabelNode;

import lisp.lang.Symbol;

public class BlockBinding
{
    private final Symbol blockName;
    private final Class<?> returnClass;
    private final LabelNode label;

    public BlockBinding (final Symbol blockName, final Class<?> returnClass, final LabelNode label)
    {
	this.blockName = blockName;
	this.returnClass = returnClass;
	this.label = label;
    }

    public Symbol getBlockName ()
    {
	return blockName;
    }

    public Class<?> getReturnClass ()
    {
	return returnClass;
    }

    public LabelNode getLabel ()
    {
	return label;
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
