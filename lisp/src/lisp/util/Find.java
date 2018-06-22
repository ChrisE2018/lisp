
package lisp.util;

import java.util.function.Predicate;

public class Find<V>
{
    public V find (final Iterable<V> items, final Predicate<V> predicate)
    {
	return find (items, null, predicate);
    }

    public V find (final Iterable<V> items, final V defaultValue, final Predicate<V> predicate)
    {
	for (final V object : items)
	{
	    if (predicate.test (object))
	    {
		return object;
	    }
	}
	return defaultValue;
    }

    // @Override
    // public String toString ()
    // {
    // final StringBuilder buffer = new StringBuilder ();
    // buffer.append ("#<");
    // buffer.append (getClass ().getSimpleName ());
    // buffer.append (" ");
    // buffer.append (System.identityHashCode (this));
    // buffer.append (">");
    // return buffer.toString ();
    // }
}
