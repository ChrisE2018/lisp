
package lisp.describe;

import java.lang.reflect.*;

import lisp.lang.Describer;
import lisp.util.MultiMap;

public class ObjectDescriber implements Describer
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
	final Class<?> cls = target.getClass ();
	result.put ("Class", cls);
	result.put ("Hashcode", target.hashCode ());
	final Method[] methods = cls.getMethods ();
	for (final Method method : methods)
	{
	    if (method.getParameterTypes ().length == 0)
	    {
		final String methodName = method.getName ();
		if (methodName.startsWith ("get"))
		{
		    try
		    {
			final Object value = method.invoke (target);
			result.put (methodName.substring (3), value);
		    }
		    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
		    {
			e.printStackTrace ();
		    }
		}
		if (methodName.startsWith ("is"))
		{
		    try
		    {
			final Object value = method.invoke (target);
			result.put (methodName.substring (2), value);
		    }
		    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
		    {
			e.printStackTrace ();
		    }
		}
	    }
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
