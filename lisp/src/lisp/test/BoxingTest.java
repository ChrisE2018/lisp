
package lisp.test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import lisp.util.Boxing;

class BoxingTest
{
    private static Class<?>[] PRIMITIVE_CLASSES =
        {boolean.class, char.class, byte.class, short.class, int.class, long.class, float.class, double.class};

    @Test
    void test1 ()
    {
	final Boxing boxing = new Boxing ();
	for (final Class<?> p : PRIMITIVE_CLASSES)
	{
	    final Class<?> w = boxing.boxedClass (p);
	    assertEquals (w, boxing.boxedClass (p));
	}
    }

    @Test
    void test2 ()
    {
	final Boxing boxing = new Boxing ();
	for (final Class<?> p : PRIMITIVE_CLASSES)
	{
	    final Class<?> w = boxing.boxedClass (p);
	    assertEquals (p, boxing.unboxedClass (w));
	}
    }

    @Test
    void test3 ()
    {
	final Boxing boxing = new Boxing ();
	for (final Class<?> p : PRIMITIVE_CLASSES)
	{
	    final Class<?> w = boxing.boxedClass (p);
	    assertEquals (w, boxing.boxedClass (w));
	}
    }

    @Test
    void test4 ()
    {
	final Boxing boxing = new Boxing ();
	for (final Class<?> p : PRIMITIVE_CLASSES)
	{
	    assertEquals (p, boxing.unboxedClass (p));
	}
    }
}
