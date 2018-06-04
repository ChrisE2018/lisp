
package lisp.cc4;

import java.util.*;

import org.objectweb.asm.tree.LabelNode;

public class CompileResultSet
{
    private final List<CompileResult> results;

    public CompileResultSet (final List<CompileResult> results)
    {
	this.results = results;
    }

    /** Constructor for fixed result. */
    public CompileResultSet (final CompileResult... result)
    {
	results = new ArrayList<CompileResult> ();
	for (final CompileResult r : result)
	{
	    results.add (r);
	}
    }

    public void addImplicitCompileResult (final LabelNode l1, final Object value)
    {
	results.add (new ImplicitCompileResult (l1, value));
    }

    public void addExplictCompileResult (final LabelNode l1, final Class<?> kind)
    {
	results.add (new ExplicitCompileResult (l1, kind));
    }

    public List<CompileResult> getResults ()
    {
	return results;
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
