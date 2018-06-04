
package lisp.eval;

import lisp.Symbol;

public class The extends Definer
{
    /**
     * Try to convert an object to a specified type.
     *
     * @param type A Symbol, String or Class to specify the desired type.
     * @param arg The object to be converted.
     * @return The converted object or null if it cannot be converted.
     * @throws Exception
     */
    @DefineLisp (special = true, classname = "lisp.special.TheFunction")
    public Object the (final LexicalContext context, final Object type, final Object arg) throws Exception
    {
	return coerce (type, context.eval (arg));
    }

    @DefineLisp
    public Object coerce (final Object type, final Object arg)
    {
	if (type instanceof Symbol)
	{
	    final Symbol t = (Symbol)type;
	    if (t.is ("byte"))
	    {
		return (byte)((Number)arg).byteValue ();
	    }
	    if (t.is ("char"))
	    {
		// [TODO] Is this right?
		return (char)((Number)arg).intValue ();
	    }
	    if (t.is ("short"))
	    {
		return ((Number)arg).shortValue ();
	    }
	    if (t.is ("int"))
	    {
		return ((Number)arg).intValue ();
	    }
	    if (t.is ("long"))
	    {
		return ((Number)arg).longValue ();
	    }
	    if (t.is ("float"))
	    {
		return ((Number)arg).floatValue ();
	    }
	    if (t.is ("double"))
	    {
		return ((Number)arg).doubleValue ();
	    }

	    return coerce (t.getName (), arg);
	}
	else if (type instanceof Class)
	{
	    final Class<?> c = (Class<?>)type;
	    return c.cast (arg);
	}
	else if (type instanceof String)
	{
	    final String t = (String)type;
	    try
	    {
		final Class<?> c = Class.forName (t);
		return c.cast (arg);
	    }
	    catch (final ClassNotFoundException e)
	    {
	    }
	    if (t.indexOf (".") < 0)
	    {
		try
		{
		    final Class<?> c = Class.forName ("java.lang." + t);
		    return c.cast (arg);
		}
		catch (final ClassNotFoundException e)
		{
		}
	    }
	}
	return null;
    }

    /**
     * Compare all arguments for equality. Numbers are compared numerically and other objects are
     * compared with the Java equals method.
     */
    @DefineLisp
    public boolean equalp (final Object a, final Object... arguments)
    {
	// For all Number implementations, prefer the Type.valueOf(value) to new Type(value). They
	// all provide caches, which will save memory in large scale applications.
	for (int i = 0; i < arguments.length; i++)
	{
	    if (!isequalp (a, arguments[i]))
	    {
		return false;
	    }
	}
	return true;
    }

    public boolean isequalp (final Object a, final Object b)
    {
	if (a instanceof Number)
	{
	    if (b instanceof Number)
	    {
		return eql (a, b);
	    }
	    return false;
	}
	return a.equals (b);
    }

    @DefineLisp (name = "=")
    public boolean eql (final Object a, final Object... arguments)
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    if (!eql (a, arguments[i]))
	    {
		return false;
	    }
	}
	return true;
    }

    @DefineLisp (name = "<>")
    public boolean neql (final Object a, final Object b)
    {
	return !eql (a, b);
    }

    private boolean eql (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return eql ((int)a, b);
	}
	else if (a instanceof Long)
	{
	    return eql ((long)a, b);
	}
	else if (a instanceof Double)
	{
	    return eql ((double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    return eql (aa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    return eql (aa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    return eql (aa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    private boolean eql (final int a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a == (int)b;
	}
	else if (b instanceof Long)
	{
	    return a == (long)b;
	}
	else if (b instanceof Double)
	{
	    return (double)a == (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a == (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a == (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return (float)a == (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private boolean eql (final long a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a == (int)b;
	}
	else if (b instanceof Long)
	{
	    return a == (long)b;
	}
	else if (b instanceof Double)
	{
	    return (double)a == (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a == (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a == (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return (float)a == (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private boolean eql (final double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a == (double)(Integer)b;
	}
	else if (b instanceof Long)
	{
	    return a == (long)b;
	}
	else if (b instanceof Double)
	{
	    return a == (double)b;
	}
	else if (b instanceof Short)
	{
	    return a == (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a == (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a == (double)(Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
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
