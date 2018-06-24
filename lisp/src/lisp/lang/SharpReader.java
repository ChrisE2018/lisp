
package lisp.lang;

import java.io.IOException;
import java.util.*;
import java.util.function.BiFunction;

/**
 * Class to read sharp notation. This is designed to be an extensible notation for reading special
 * things.
 */
public class SharpReader
{
    private static Map<Character, BiFunction<LispReader, LispStream, Object>> quickDispatchHandlers = new HashMap<> ();

    private static Map<String, BiFunction<LispReader, LispStream, Object>> dispatchHandlers = new HashMap<> ();

    private final LispReader lispReader;

    /** Create a SharpReader for each LispReader. */
    public SharpReader (final LispReader lispReader)
    {
	this.lispReader = lispReader;
    }

    public Object readSharpForm (final LispStream stream) throws IOException
    {
	// #Definer(lisp.special.Or)
	char chr = stream.peek ();
	final BiFunction<LispReader, LispStream, Object> handler = quickDispatchHandlers.get (chr);
	if (handler != null)
	{
	    return handler.apply (lispReader, stream);
	}
	if (Character.isAlphabetic (chr))
	{
	    final StringBuilder buffer = new StringBuilder ();
	    while (Character.isAlphabetic (chr))
	    {
		stream.read (chr);
		buffer.append (chr);
		chr = stream.peek ();
	    }
	    final String key = buffer.toString ();
	    final BiFunction<LispReader, LispStream, Object> fn = dispatchHandlers.get (key);
	    if (fn != null)
	    {
		return fn.apply (lispReader, stream);
	    }
	    throw new IllegalArgumentException ("No sharp handler for " + key);
	}
	throw new IllegalArgumentException ("No sharp handler for " + chr);
    }

    public static void addQuickDispatchHandler (final char chr, final BiFunction<LispReader, LispStream, Object> handler)
    {
	quickDispatchHandlers.put (chr, handler);
    }

    public static void addDispatchHandler (final String key, final BiFunction<LispReader, LispStream, Object> handler)
    {
	dispatchHandlers.put (key, handler);
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
