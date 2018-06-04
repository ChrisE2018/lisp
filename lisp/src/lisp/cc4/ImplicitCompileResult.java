
package lisp.cc4;

import org.objectweb.asm.tree.LabelNode;

public class ImplicitCompileResult extends CompileResult
{
    private final Object value;

    public ImplicitCompileResult (final LabelNode l, final Object value)
    {
	super (l);
	this.value = value;
    }

    public Object getValue ()
    {
	return value;
    }

    @Override
    public CompileResult getJumpTo (final LabelNode ll)
    {
	return new ImplicitCompileResult (ll, value);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (" ");
	buffer.append (value);
	buffer.append (">");
	return buffer.toString ();
    }
}
