
package lisp.cc4;

import java.util.*;

import org.objectweb.asm.tree.LabelNode;

public class CompileResults
{
    private final List<CompileResult> results;

    public CompileResults (final List<CompileResult> results)
    {
	this.results = results;
    }

    /** Constructor for fixed result. */
    public CompileResults (final CompileResult... result)
    {
	results = new ArrayList<CompileResult> ();
	for (final CompileResult r : result)
	{
	    results.add (r);
	}
    }

    public void addImplicitCompileResult (final LabelNode l1, final Object value)
    {
	add (new ImplicitResult (l1, value));
    }

    public void addExplicitCompileResult (final LabelNode l1, final Class<?> kind)
    {
	add (new ExplicitResult (l1, kind));
    }

    public void add (final CompileResult cr)
    {
	// Don't add duplicates
	for (final CompileResult r : results)
	{
	    if (r.equals (cr))
	    {
		r.addLabels (cr.getLabels ());
		return;
	    }
	}
	results.add (cr);
    }

    public List<CompileResult> getResults ()
    {
	return Collections.unmodifiableList (results);
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
	if (results != null)
	{
	    for (final CompileResult cr : results)
	    {
		buffer.append (" ");
		buffer.append (cr);
	    }
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
