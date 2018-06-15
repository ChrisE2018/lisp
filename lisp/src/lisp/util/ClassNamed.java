
package lisp.util;

import lisp.Symbol;

public class ClassNamed
{
    private static Object[][] CLASS_MAP =
        {
         {"byte", byte.class},
         {"short", short.class},
         {"int", int.class},
         {"long", long.class},
         {"float", float.class},
         {"double", double.class},
         {"char", char.class},
         {"boolean", boolean.class}};

    public Class<?> getClass (final Object reference)
    {
	if (reference instanceof Class)
	{
	    return (Class<?>)reference;
	}
	if (reference instanceof Symbol)
	{
	    return getClass (((Symbol)reference).getName ());
	}
	for (final Object[] clause : CLASS_MAP)
	{
	    if (clause[0].equals (reference))
	    {
		return (Class<?>)clause[1];
	    }
	}
	return null;
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
