
package lisp;

import java.util.*;

public interface Describer
{
    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target The object to describe.
     * @return The map containing key value pairs for describe.
     */
    default public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	getDescriberValues (result, target);
	return result;
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    default public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	result.put ("Class", target.getClass ());
    }

    /** Convert an object to a string for printing. */
    public default String getDescriberString (final Object target)
    {
	if (target == null)
	{
	    return "null";
	}
	else
	{
	    return target.toString ();
	}
    }

    /**
     * Get optional documentation string for an object described slot.
     *
     * @param target
     * @param key
     */
    public default String getDescriberDocumentation (final Object target, final String key)
    {
	return null;
    }
}
