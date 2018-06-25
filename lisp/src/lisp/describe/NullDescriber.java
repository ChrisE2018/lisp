
package lisp.describe;

import lisp.lang.Describer;
import lisp.util.MultiMap;

public class NullDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	result.put ("Class", null);
    }
}
