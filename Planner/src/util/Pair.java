
package util;

import lisp.lang.Describer;
import lisp.util.MultiMap;

/** Type-safe ordered pair. */
public class Pair<A, B> implements Describer
{
    private final A a;
    private final B b;

    public Pair (final A a, final B b)
    {
	this.a = a;
	this.b = b;
    }

    public A getFirst ()
    {
	return a;
    }

    public B getSecond ()
    {
	return b;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (a);
	buffer.append (" ");
	buffer.append (b);
	buffer.append (">");
	return buffer.toString ();
    }

    @Override
    public MultiMap<String, Object> getDescriberValues (final Object target)
    {
	final MultiMap<String, Object> result = new MultiMap<> ();
	result.put ("First", a);
	result.put ("Second", b);
	return result;
    }
}
