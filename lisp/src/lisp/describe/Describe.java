
package lisp.describe;

import java.util.*;
import java.util.Map.Entry;

import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.MultiMap;

public class Describe
{
    private DescriberFactory factory;

    public Describe ()
    {
	factory = new DescriberFactory ();
    }

    public Describe (final DescriberFactory factory)
    {
	this.factory = factory != null ? factory : new DescriberFactory ();
    }

    public DescriberFactory getFactory ()
    {
	return factory;
    }

    public void setFactory (final DescriberFactory factory)
    {
	this.factory = factory;
    }

    public Object describe (final Object arg)
    {
	final Describer describer = factory.getDescriber (arg);
	final Package pkg = PackageFactory.getCurrentPackage ();
	int index = 0;
	final MultiMap<String, Object> description = describer.getDescriberValues (arg);
	for (final Entry<String, Collection<Object>> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    final String key = entry.getKey ();
	    final Set<Object> values = new LinkedHashSet<> (entry.getValue ());
	    for (final Object value : values)
	    {
		++index;
		final Describer valueDescriber = factory.getDescriber (value);
		final String valueString =
		    (valueDescriber == null) ? value.toString () : valueDescriber.getDescriberString (value);
		final String doc = describer.getDescriberDocumentation (arg, key);
		final Symbol symbol = pkg.internSymbol (String.format ("d%d", index));
		symbol.setValue (value);
		final String type = value == null ? "" : "(" + value.getClass ().getSimpleName () + ") ";
		if (doc != null)
		{
		    System.out.printf ("%3s %30s: %-25s %s\n", symbol, type + key, valueString, doc);
		}
		else
		{
		    System.out.printf ("%3s %30s: %-25s\n", symbol, type + key, valueString);
		}
	    }
	}
	return arg;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (factory);
	buffer.append (">");
	return buffer.toString ();
    }
}
