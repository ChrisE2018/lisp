
package lisp;

import java.util.*;

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
