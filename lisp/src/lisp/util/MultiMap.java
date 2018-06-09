
package lisp.util;

import java.util.*;

/**
 * Multimap in Java. Source: http://www.techiedelight.com/implement-multimap-java/
 *
 * @author unknown
 * @param <K>
 * @param <V>
 */
public class MultiMap<K, V>
{
    private final Map<K, Collection<V>> map = new LinkedHashMap<> ();

    /**
     * Add the specified value with the specified key in this multimap.
     */
    public void put (final K key, final V value)
    {
	if (map.get (key) == null)
	{
	    map.put (key, new ArrayList<V> ());
	}

	map.get (key).add (value);
    }

    /**
     * Associate the specified key with the given value if not already associated with a value
     */
    public void putIfAbsent (final K key, final V value)
    {
	if (map.get (key) == null)
	{
	    map.put (key, new ArrayList<> ());
	}

	// if value is absent, insert it
	if (!map.get (key).contains (value))
	{
	    map.get (key).add (value);
	}
    }

    /**
     * Returns the Collection of values to which the specified key is mapped, or null if this
     * multimap contains no mapping for the key.
     */
    public Collection<V> get (final Object key)
    {
	return map.get (key);
    }

    /**
     * Returns a Set view of the keys contained in this multimap.
     */
    public Set<K> keySet ()
    {
	return map.keySet ();
    }

    /**
     * Returns a Set view of the mappings contained in this multimap.
     */
    public Set<Map.Entry<K, Collection<V>>> entrySet ()
    {
	return map.entrySet ();
    }

    /**
     * Returns a Collection view of Collection of the values present in this multimap.
     */
    public Collection<Collection<V>> values ()
    {
	return map.values ();
    }

    /**
     * Returns true if this multimap contains a mapping for the specified key.
     */
    public boolean containsKey (final Object key)
    {
	return map.containsKey (key);
    }

    /**
     * Removes the mapping for the specified key from this multimap if present. and returns the
     * Collection of previous values associated with key, or null if there was no mapping for key.
     */
    public Collection<V> remove (final Object key)
    {
	return map.remove (key);
    }

    /**
     * Returns the number of key-value mappings in this map.
     */
    public int size ()
    {
	int size = 0;
	for (final Collection<V> value : map.values ())
	{
	    size += value.size ();
	}
	return size;
    }

    /**
     * Returns true if this map contains no key-value mappings.
     */
    public boolean isEmpty ()
    {
	return map.isEmpty ();
    }

    /**
     * Removes all of the mappings from this map.
     */
    public void clear ()
    {
	map.clear ();
    }

    /**
     * Removes the entry for the specified key only if it is currently mapped to the specified value
     * and return true if removed
     */
    public boolean remove (final K key, final V value)
    {
	if (map.get (key) != null)
	{
	    return map.get (key).remove (value);
	}

	return false;
    }

    /**
     * Replaces the entry for the specified key only if currently mapped to the specified value and
     * return true if replaced
     */
    public boolean replace (final K key, final V oldValue, final V newValue)
    {

	if (map.get (key) != null)
	{
	    if (map.get (key).remove (oldValue))
	    {
		return map.get (key).add (newValue);
	    }
	}
	return false;
    }
}

class MapUtils
{
    // Multimap implementation in Java
    public static void main (final String[] args)
    {
	// create a Multimap from past US presidents list
	final MultiMap<String, String> multimap = new MultiMap ();

	multimap.put ("Zachary", "Taylor");
	multimap.put ("John", "Adams");
	multimap.put ("John", "Tyler");
	multimap.put ("John", "Kennedy");
	multimap.put ("George", "Washington");
	multimap.put ("George", "Bush");
	multimap.put ("Grover", "Cleveland");

	System.out.println ("--------Printing Multimap using keySet ----------\n");
	for (final String lastName : multimap.keySet ())
	{
	    System.out.println (lastName + ": " + multimap.get (lastName));
	}
    }
}
