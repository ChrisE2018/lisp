
package lisp.cc4;

import java.util.List;

import org.objectweb.asm.tree.LabelNode;

public class ImplicitResult extends CompileResult
{
    private static final TreeBoxer boxer = new TreeBoxer ();

    private final Object value;

    public ImplicitResult (final LabelNode l, final Object value)
    {
	super (l);
	this.value = value;
    }

    public ImplicitResult (final List<org.objectweb.asm.tree.LabelNode> labels, final Object value)
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
	if (value == null)
	{
	    return void.class;
	}
	final Class<?> ec = value.getClass ();
	final Class<?> p = boxer.getUnboxedClass (ec);
	return p == null ? ec : p;
    }

    @Override
    public boolean equals (final Object o)
    {
	if (o instanceof ImplicitResult)
	{
	    final ImplicitResult icr = (ImplicitResult)o;

	    if (icr.value == null)
	    {
		return value == null;
	    }
	    return icr.value.equals (value);
	}
	return false;
    }

    @Override
    public int hashCode ()
    {
	if (value == null)
	{
	    return 0;
	}
	return value.hashCode ();
    }

    @Override
    public CompileResult getJumpTo (final LabelNode ll)
    {
	return new ImplicitResult (ll, value);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	if (value == null)
	{
	    buffer.append ("null");
	}
	else
	{
	    buffer.append (value.getClass ().getSimpleName ());
	    buffer.append (" ");
	    buffer.append (value);
	}
	final List<org.objectweb.asm.tree.LabelNode> labels = getLabels ();
	if (labels != null)
	{
	    for (final LabelNode label : labels)
	    {
		buffer.append (" @");
		buffer.append (label);
	    }
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
