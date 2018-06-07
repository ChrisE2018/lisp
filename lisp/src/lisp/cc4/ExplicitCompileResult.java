
package lisp.cc4;

import org.objectweb.asm.tree.LabelNode;

public class ExplicitCompileResult extends CompileResult
{
    private final Class<?> resultClass;

    public ExplicitCompileResult (final Class<?> resultClass)
    {
	this.resultClass = resultClass;
    }

    public ExplicitCompileResult (final LabelNode l, final Class<?> resultClass)
    {
	super (l);
	this.resultClass = resultClass;
    }

    @Override
    public Class<?> getResultClass ()
    {
	return resultClass;
    }

    @Override
    public CompileResult getJumpTo (final LabelNode ll)
    {
	return new ExplicitCompileResult (ll, resultClass);
    }

    @Override
    public boolean equals (final Object o)
    {
	if (o instanceof ExplicitCompileResult)
	{
	    final ExplicitCompileResult ecr = (ExplicitCompileResult)o;
	    return ecr.resultClass.equals (resultClass);
	}
	return false;
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
	buffer.append (resultClass);
	for (final LabelNode label : getLabels ())
	{
	    buffer.append (" @");
	    buffer.append (label);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
