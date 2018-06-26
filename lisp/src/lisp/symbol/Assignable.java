/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import lisp.lang.Symbol;
import lisp.util.Boxing;

public class Assignable
{
    private static Boxing boxing = new Boxing ();

    /**
     * Determine if a formal parameter can be assigned from an actual parameter. Null values cannot
     * be assigned to primitive types. Primitive types will support widening conversions, but not
     * allow narrowing. The Symbol class is treated specially and can be assigned to a String
     * parameter.
     *
     * @param toClass The class of the formal parameter.
     * @param from The actual parameter value.
     * @return True if the assignment is possible.
     */
    public boolean isAssignableFrom (final Class<?> toClass, final Object from)
    {
	if (from == null)
	{
	    return !toClass.isPrimitive ();
	}
	final Class<?> fromClass = from.getClass ();
	return isAssignableFrom (toClass, fromClass);
    }

    public boolean isAssignableFrom (final Class<?> toClass, final Class<?> fromClass)
    {
	if (toClass.isAssignableFrom (fromClass))
	{
	    return true;
	}
	if (toClass == String.class && fromClass == Symbol.class)
	{
	    return true;
	}
	if (toClass.isPrimitive ())
	{
	    if (fromClass.isPrimitive ())
	    {
		return isPrimitiveAssignableFrom (toClass, fromClass);
	    }
	    final Class<?> fromUnwrapped = boxing.unboxedClass (fromClass);
	    return isPrimitiveAssignableFrom (toClass, fromUnwrapped);
	}
	if (fromClass.isPrimitive ())
	{
	    return toClass.isAssignableFrom (boxing.boxedClass (fromClass));
	}
	return false;
    }

    private boolean isPrimitiveAssignableFrom (final Class<?> toClass, final Class<?> fromClass)
    {
	// No simple solution:
	// https://stackoverflow.com/questions/1704634/simple-way-to-get-wrapper-class-type-in-java
	if (toClass == int.class)
	{
	    if (fromClass == int.class || fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == boolean.class)
	{
	    if (fromClass == boolean.class)
	    {
		return true;
	    }
	}
	else if (toClass == double.class)
	{
	    if (fromClass == double.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class
	        || fromClass == float.class || fromClass == long.class)
	    {
		return true;
	    }
	}
	else if (toClass == long.class)
	{
	    if (fromClass == long.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == char.class)
	{
	    if (fromClass == char.class)
	    {
		return true;
	    }
	}
	else if (toClass == byte.class)
	{
	    if (fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == short.class)
	{
	    if (fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == float.class)
	{
	    if (fromClass == float.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class
	        || fromClass == float.class || fromClass == long.class)
	    {
		return true;
	    }
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
