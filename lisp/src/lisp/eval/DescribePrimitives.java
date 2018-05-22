
package lisp.eval;

import java.util.*;
import java.util.Map.Entry;

import lisp.*;
import lisp.Package;

public class DescribePrimitives extends Definer
{
    @DefineLisp
    public Object describe (final Object arg)
    {
	System.out.printf ("Describe: %s \n", arg);
	if (arg != null)
	{
	    final Describer d = getDescriber (arg);
	    if (d != null)
	    {
		describe (d, arg);
	    }
	    else
	    {
		System.out.printf ("Class: %s \n", arg.getClass ().getCanonicalName ());
	    }
	}
	return Boolean.FALSE;
    }

    private Describer getDescriber (final Object arg)
    {
	if (arg instanceof Describer)
	{
	    return (Describer)arg;
	}
	if (arg instanceof List)
	{
	    return new LispList ();
	}
	// [TODO] Java bean describer
	return null;
    }

    private void describe (final Describer d, final Object arg)
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	int index = 0;
	final Map<String, Object> description = d.getDescriberValues (arg);
	for (final Entry<String, Object> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    ++index;
	    final String key = entry.getKey ();
	    final Object value = entry.getValue ();
	    final String doc = d.getDescriberDocumentation (arg, key);
	    final Symbol symbol = pkg.internSymbol (String.format ("d%d", index));
	    symbol.setValue (value);
	    if (doc != null)
	    {
		System.out.printf ("[%5s] %s: %s %s\n", symbol, key, value, doc);
	    }
	    else
	    {
		final String type = value == null ? "null" : value.getClass ().getSimpleName ();
		System.out.printf ("[%5s] %s: %s (%s)\n", symbol, key, value, type);
	    }
	}
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
