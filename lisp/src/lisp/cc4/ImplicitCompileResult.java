
package lisp.cc4;

import org.objectweb.asm.tree.LabelNode;

public class ImplicitCompileResult extends CompileResult
{
    private static final TreeBoxer boxer = new TreeBoxer ();

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
	    return icr.getLabel () == getLabel () && icr.value.equals (value);
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
	buffer.append (" ");
	buffer.append (getLabel ());
	buffer.append (">");
	return buffer.toString ();
    }
}
