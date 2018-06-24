
package lisp.eval;

import java.io.IOException;
import java.util.List;
import java.util.function.BiFunction;

import lisp.lang.*;

public class DefinerSharpReader implements BiFunction<LispReader, LispStream, Object>
{
    /** Read an object that is a subclass of Definer. */
    @Override
    public Object apply (final LispReader reader, final LispStream stream)
    {
	try
	{
	    final Object object = reader.read (stream);
	    if (object instanceof List)
	    {
		final List<?> list = (List<?>)object;
		final Object c = list.get (0);
		if (c instanceof Class<?>)
		{
		    @SuppressWarnings ("unchecked")
		    final Class<? extends Definer> cls = (Class<? extends Definer>)list.get (0);
		    final Object result = Definer.getDefiner (cls);
		    if (result != null)
		    {
			return result;
		    }
		    throw new Error ("Invalid #Definer class: " + cls);
		}
		throw new Error ("#Definer requires a class: " + c);
	    }
	    throw new Error ("#Definer requires a list parameter: " + object);
	}
	catch (final IOException e)
	{
	    throw new Error ("Error reading #Definer", e);
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
