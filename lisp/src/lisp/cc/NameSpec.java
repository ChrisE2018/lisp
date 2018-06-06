
package lisp.cc;

import java.util.List;

import org.objectweb.asm.Type;

import lisp.Symbol;

public class NameSpec
{
    /**
     * Return the variable from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <type> <variable>)
     * @return The <variable>.
     */
    public static Symbol getVariableName (final Object nameSpec)
    {
	if (nameSpec instanceof Symbol)
	{
	    return (Symbol)nameSpec;
	}
	final List<?> spec = (List<?>)nameSpec;
	return (Symbol)spec.get (2);
    }

    /**
     * Return the <class> from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <class> <variable>)
     * @return The <class> which is Object.class if unspecified.
     */
    public static Class<?> getVariableClass (final Object nameSpec)
    {
	return getVariableClass (nameSpec, Object.class);
    }

    /**
     * Return the <class> from a nameSpec.
     *
     * @param nameSpec Either a <variable> or form (the <class> <variable>)
     * @return The <class> which is Object.class if unspecified.
     */
    public static Class<?> getVariableClass (final Object nameSpec, final Class<?> defaultType)
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
	    return getStringClass ((String)type);
	}
	if (type instanceof Symbol)
	{
	    // Try with the qualified name of the symbol
	    return getStringClass (((Symbol)type).getName ());
	}
	return defaultType;
    }

    /**
     * Convert a string to a class name. Assume this names a primitive class in java.lang if not
     * fully qualified.
     *
     * @param type
     * @return
     */
    public static Class<?> getStringClass (final String type)
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
	    if (type.equals ("float"))
	    {
		return float.class;
	    }
	    return Class.forName (type);
	}
	catch (final ClassNotFoundException e)
	{
	    if (type.indexOf ('.') < 0)
	    {
		try
		{
		    final char ch = type.charAt (0);
		    return Class.forName ("java.lang." + Character.toUpperCase (ch) + type.substring (1));
		}
		catch (final ClassNotFoundException ee)
		{
		}

	    }
	}
	return Object.class;
    }

    public static Type getVariableType (final Object nameSpec)
    {
	return Type.getType (getVariableClass (nameSpec));
    }

    public static String getVariableDescriptor (final Object nameSpec)
    {
	return getVariableType (nameSpec).getDescriptor ();
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
