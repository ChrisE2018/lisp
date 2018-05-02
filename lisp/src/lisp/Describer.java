
package lisp;

import java.util.Map;

public interface Describer
{
    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target
     * @return
     */
    public Map<String, Object> getDescriberValues (Object target);

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
