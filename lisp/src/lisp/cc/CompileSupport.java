
package lisp.cc;

import java.util.List;

import org.objectweb.asm.Type;

import lisp.Symbol;

public class CompileSupport
{
    public static Symbol getFunctionName (final Object nameSpec)
    {
	if (nameSpec instanceof Symbol)
	{
	    return (Symbol)nameSpec;
	}
	final List<?> spec = (List<?>)nameSpec;
	return (Symbol)spec.get (2);
    }

    public static Class<?> getNameType (final Object nameSpec)
    {
	if (nameSpec instanceof Symbol)
	{
	    return Object.class;
	}

	final List<?> spec = (List<?>)nameSpec;
	final Object type = spec.get (1);
	if (type instanceof Class)
	{
	    return (Class<?>)type;
	}
	if (type instanceof String)
	{
	    return getNameType ((String)type);
	}
	if (type instanceof Symbol)
	{
	    // Try with the qualified name of the symbol
	    return getNameType (((Symbol)type).getName ());
	}
	return Object.class;
    }

    public static Class<?> getNameType (final String type)
    {
	try
	{
	    return Class.forName (type);
	}
	catch (final ClassNotFoundException e)
	{
	    if (type.indexOf ('.') < 0)
	    {
		try
		{
		    return Class.forName ("java.lang." + type);
		}
		catch (final ClassNotFoundException ee)
		{
		}

	    }
	}
	return Object.class;
    }

    public static String getNameTypeDescriptor (final Object nameSpec)
    {
	return Type.getType (getNameType (nameSpec)).getDescriptor ();
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
