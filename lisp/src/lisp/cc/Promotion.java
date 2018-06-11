
package lisp.cc;

import java.util.*;

public class Promotion
{
    private static Class<?>[][] PROMOTION_TABLE =
        {
         {boolean.class, Boolean.class, Object.class},
         {Boolean.class, boolean.class, Object.class},
         {byte.class, Byte.class, char.class, Character.class, short.class, Short.class, int.class, Integer.class, long.class,
          Long.class, Object.class},
         {Byte.class, byte.class, char.class, Character.class, short.class, Short.class, int.class, Integer.class, long.class,
          Long.class, Object.class},
         {short.class, Short.class, int.class, Integer.class, long.class, Long.class, Object.class},
         {Short.class, short.class, int.class, Integer.class, long.class, Long.class, Object.class},
         {int.class, Integer.class, long.class, Long.class, Object.class},
         {Integer.class, int.class, long.class, Long.class, Object.class},
         {long.class, Long.class, Object.class},
         {Long.class, long.class, Object.class},
         {float.class, Float.class, double.class, Double.class, Object.class},
         {Float.class, float.class, double.class, Double.class, Object.class},
         {double.class, Double.class, Object.class},
         {Double.class, double.class, Object.class},
         {String.class,
          // Symbol.class,
          Object.class},
         {Object.class}};

    private final Map<Class<?>, List<Class<?>>> promotionTable = new HashMap<Class<?>, List<Class<?>>> ();

    public Promotion ()
    {
	for (final Class<?>[] chain : PROMOTION_TABLE)
	{
	    final List<Class<?>> chainList = new ArrayList<Class<?>> ();
	    final Class<?> key = chain[0];
	    for (final Class<?> parent : chain)
	    {
		chainList.add (parent);
	    }
	    promotionTable.put (key, Collections.unmodifiableList (chainList));
	}
    }

    public Map<Class<?>, List<Class<?>>> getPromotionTable ()
    {
	return promotionTable;
    }

    public List<Class<?>> getPromotion (final Class<?> type)
    {
	return promotionTable.get (type);
    }

    public List<Class<?>> getValidPromotions (final Class<?> type, final Class<?> valueType)
    {
	final List<Class<?>> parents = promotionTable.get (type);
	for (final Class<?> p : parents)
	{
	    if (!valueType.isAssignableFrom (p))
	    {
		// Copy and filter
		final List<Class<?>> result = new ArrayList<Class<?>> ();
		for (final Class<?> pp : parents)
		{
		    if (valueType.isAssignableFrom (pp))
		    {
			result.add (pp);
		    }
		}
		return Collections.unmodifiableList (result);
	    }
	}
	return parents;
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
