
package lisp.describe;

import java.lang.reflect.Array;
import java.util.Map;

import lisp.Describer;

public class ArrayDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	return "[" + getArrayString (target, 50) + "]";
    }

    private String getArrayString (final Object array, final int maxLength)
    {
	final StringBuilder buffer = new StringBuilder ();
	final int length = Array.getLength (array);
	for (int i = 0; i < length; i++)
	{
	    if (i > 0)
	    {
		buffer.append (", ");
	    }
	    if (buffer.length () > maxLength)
	    {
		buffer.append ("...");
		return buffer.toString ();
	    }
	    buffer.append (Array.get (array, i));
	}
	return buffer.toString ();
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object array)
    {
	final int length = Array.getLength (array);
	for (int i = 0; i < length; i++)
	{
	    final Object element = Array.get (array, i);
	    result.put (String.valueOf (i), element);
	}
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
