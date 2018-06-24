
package lisp.describe;

import java.util.*;
import java.util.Map.Entry;

import org.objectweb.asm.tree.*;

import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.MultiMap;

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
	if (arg == null)
	{
	    return new ObjectDescriber ();
	}
	if (arg instanceof Describer)
	{
	    return (Describer)arg;
	}
	if (arg instanceof List)
	{
	    return new LispList ();
	}
	if (arg instanceof Class)
	{
	    return new ClassDescriber ();
	}
	else if (arg instanceof ClassNode)
	{
	    return new ClassNodeDescriber ();
	}
	else if (arg instanceof MethodNode)
	{
	    return new MethodNodeDescriber ();
	}
	else if (arg instanceof InsnList)
	{
	    return new InsnListDescriber ();
	}
	else if (arg instanceof AbstractInsnNode)
	{
	    return new AbstractInsnNodeDescriber ();
	}
	else
	{
	    final Class<?> cls = arg.getClass ();
	    if (cls.isArray ())
	    {
		return new ArrayDescriber ();
	    }
	    else
	    {
		return new ObjectDescriber ();
	    }
	}
    }

    private void describe (final Describer d, final Object arg)
    {
	final Package pkg = PackageFactory.getCurrentPackage ();
	int index = 0;
	final MultiMap<String, Object> description = d.getDescriberValues (arg);
	for (final Entry<String, Collection<Object>> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    final String key = entry.getKey ();
	    for (final Object value : entry.getValue ())
	    {
		++index;
		final Describer valueDescriber = getDescriber (value);
		final String valueString =
		    (valueDescriber == null) ? value.toString () : valueDescriber.getDescriberString (value);
		final String doc = d.getDescriberDocumentation (arg, key);
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
    }
}
