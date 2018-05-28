
package lisp.cc;

import java.util.List;

import org.objectweb.asm.Type;

import lisp.Symbol;

public class CompileSupport
{
    /**
     * Return the variable from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <type> <variable>)
     * @return The <variable>.
     */
    public static Symbol getNameVariable (final Object nameSpec)
    {
	if (nameSpec instanceof Symbol)
	{
	    return (Symbol)nameSpec;
	}
	final List<?> spec = (List<?>)nameSpec;
	return (Symbol)spec.get (2);
    }

    /**
     * Return the <type> from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <type> <variable>)
     * @return The <type> which is Object.class if unspecified.
     */
    public static Class<?> getNameType (final Object nameSpec)
    {
	return getNameType (nameSpec, Object.class);
    }

    /**
     * Return the <type> from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <type> <variable>)
     * @return The <type> which is Object.class if unspecified.
     */
    public static Class<?> getNameType (final Object nameSpec, final Class<?> defaultType)
    {
	if (nameSpec instanceof Symbol)
	{
	    return defaultType;
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
	return defaultType;
    }

    public static Class<?> getNameType (final String type)
    {
	try
	{
	    if (type.equals ("boolean"))
	    {
		return boolean.class;
	    }
	    if (type.equals ("short"))
	    {
		return short.class;
	    }
	    if (type.equals ("int"))
	    {
		return int.class;
	    }
	    if (type.equals ("long"))
	    {
		return long.class;
	    }
	    if (type.equals ("double"))
	    {
		return double.class;
	    }
	    if (type.equals ("byte"))
	    {
		return byte.class;
	    }
	    if (type.equals ("char"))
	    {
		return char.class;
	    }
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
