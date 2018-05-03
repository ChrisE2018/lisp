
package lisp;

import java.util.*;

/**
 * Bidirectional map implementation. This implementation just maintains separate maps for the
 * forward and reverse lookup. Only the minimal operations required are implemented. The values must
 * be supplied in the constructor and are constant.
 *
 * @see https://stackoverflow.com/questions/1670038/does-java-have-a-hashmap-with-reverse-lookup
 * @author cre
 * @param <A>
 * @param <B>
 */
public class SimpleBiMap<A, B>
{
    private final Map<A, B> forward = new HashMap<A, B> ();
    private final Map<B, A> reverse = new HashMap<B, A> ();

    public SimpleBiMap (final Object[][] values)
    {
	for (final Object[] slot : values)
	{
	    @SuppressWarnings ("unchecked")
	    final A key = (A)slot[0];
	    @SuppressWarnings ("unchecked")
	    final B value = (B)slot[1];
	    forward.put (key, value);
	    reverse.put (value, key);
	}
    }

    public B get (final A a)
    {
	return forward.get (a);
    }

    public A getKey (final B b)
    {
	return reverse.get (b);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
