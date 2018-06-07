
package lisp.cc4;

import java.util.List;

import org.objectweb.asm.tree.LabelNode;

public class ImplicitCompileResult extends CompileResult
{
    private static final TreeBoxer boxer = new TreeBoxer ();

    private final Object value;

    public ImplicitCompileResult (final Object value)
    {
	super ();
	this.value = value;
    }

    public ImplicitCompileResult (final LabelNode l, final Object value)
    {
	super (l);
	this.value = value;
    }

    public ImplicitCompileResult (final List<LabelNode> labels, final Object value)
    {
	super (labels);
	this.value = value;
    }

    public Object getValue ()
    {
	return value;
    }

    @Override
    public Class<?> getResultClass ()
    {
	final Class<?> ec = value.getClass ();
	final Class<?> p = boxer.getUnboxedClass (ec);
	return p == null ? ec : p;
    }

    @Override
    public boolean equals (final Object o)
    {
	if (o instanceof ImplicitCompileResult)
	{
	    final ImplicitCompileResult icr = (ImplicitCompileResult)o;

	    if (icr.value == null)
	    {
		return value == null;
	    }
	    return icr.value.equals (value);
	}
	return false;
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
	buffer.append (value.getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (value);
	for (final LabelNode label : getLabels ())
	{
	    buffer.append (" @");
	    buffer.append (label);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
