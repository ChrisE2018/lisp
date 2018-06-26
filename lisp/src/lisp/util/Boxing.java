
package lisp.util;

public class Boxing
{
    /**
     * Convert the class for a primitive type to the corresponding wrapper class.
     *
     * @param primitiveClass The class of a primitive type
     * @return The corresponding wrapper class
     * @throws IllegalArgumentException If the argument is not a primitive type class.
     */
    public Class<?> boxedClass (final Class<?> primitiveClass)
    {
	if (primitiveClass == boolean.class)
	{
	    return Boolean.class;
	}
	if (primitiveClass == byte.class)
	{
	    return Byte.class;
	}
	if (primitiveClass == char.class)
	{
	    return Character.class;
	}
	if (primitiveClass == short.class)
	{
	    return Short.class;
	}
	if (primitiveClass == int.class)
	{
	    return Integer.class;
	}
	if (primitiveClass == long.class)
	{
	    return Long.class;
	}
	if (primitiveClass == float.class)
	{
	    return Float.class;
	}
	if (primitiveClass == double.class)
	{
	    return Double.class;
	}
	return primitiveClass;
    }

    /**
     * Convert the class for a wrapper type to the corresponding primitive class.
     *
     * @param wrapperClass The class of a wrapper type
     * @return The corresponding primitive class
     * @throws IllegalArgumentException If the argument is not a wrapper class for a primitive type.
     */
    public Class<?> unboxedClass (final Class<?> wrapperClass)
    {
	if (wrapperClass == Boolean.class)
	{
	    return boolean.class;
	}
	if (wrapperClass == Byte.class)
	{
	    return byte.class;
	}
	if (wrapperClass == Character.class)
	{
	    return char.class;
	}
	if (wrapperClass == Short.class)
	{
	    return short.class;
	}
	if (wrapperClass == Integer.class)
	{
	    return int.class;
	}
	if (wrapperClass == Long.class)
	{
	    return long.class;
	}
	if (wrapperClass == Float.class)
	{
	    return float.class;
	}
	if (wrapperClass == Double.class)
	{
	    return double.class;
	}
	return wrapperClass;
    }

    /**
     * Determine if the class represents a primitive type.
     *
     * @param primitiveClass The class of a any type.
     * @return True if this is a primitive type class.
     */
    public boolean isPrimitiveClass (final Class<?> primitiveClass)
    {
	if (primitiveClass == boolean.class)
	{
	    return true;
	}
	if (primitiveClass == byte.class)
	{
	    return true;
	}
	if (primitiveClass == char.class)
	{
	    return true;
	}
	if (primitiveClass == short.class)
	{
	    return true;
	}
	if (primitiveClass == int.class)
	{
	    return true;
	}
	if (primitiveClass == long.class)
	{
	    return true;
	}
	if (primitiveClass == float.class)
	{
	    return true;
	}
	if (primitiveClass == double.class)
	{
	    return true;
	}
	return false;
    }

    /**
     * Determine if the class is a wrapper type corresponding to some primitive class.
     *
     * @param wrapperClass The class of a wrapper type.
     * @return True if this is a wrapper class for a primitive type.
     */
    public boolean isBoxedClass (final Class<?> wrapperClass)
    {
	if (wrapperClass == Boolean.class)
	{
	    return true;
	}
	if (wrapperClass == Byte.class)
	{
	    return true;
	}
	if (wrapperClass == Character.class)
	{
	    return true;
	}
	if (wrapperClass == Short.class)
	{
	    return true;
	}
	if (wrapperClass == Integer.class)
	{
	    return true;
	}
	if (wrapperClass == Long.class)
	{
	    return true;
	}
	if (wrapperClass == Float.class)
	{
	    return true;
	}
	if (wrapperClass == Double.class)
	{
	    return true;
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
	buffer.append (">");
	return buffer.toString ();
    }
}
