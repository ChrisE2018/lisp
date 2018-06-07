
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
	add (new ImplicitCompileResult (l1, value));
    }

    public void addExplicitCompileResult (final LabelNode l1, final Class<?> kind)
    {
	add (new ExplicitCompileResult (l1, kind));
    }

    public void add (final CompileResult cr)
    {
	// Don't add duplicates
	if (!results.contains (cr))
	{
	    results.add (cr);
	}
    }

    public List<CompileResult> getResults ()
    {
	return results;
    }

    public CompileResult getCompileResult (final Class<?> cls)
    {
	for (final CompileResult cr : results)
	{
	    if (cr.getClass ().equals (cls))
	    {
		return cr;
	    }
	}
	return null;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	for (final CompileResult cr : results)
	{
	    buffer.append (" ");
	    buffer.append (cr);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
