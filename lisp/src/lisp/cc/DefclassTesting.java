
package lisp.cc;

import java.lang.reflect.*;

import lisp.eval.*;

public class DefclassTesting extends Definer
{
    @DefineLisp
    public void dd (final Class<?> c)
    {
	System.out.printf ("List of Declared Methods");
	for (final Method method : c.getDeclaredMethods ())
	{
	    System.out.printf ("* Method: %s", method);
	}
	System.out.println ("");
    }

    @DefineLisp
    public void tc (final Class<?> c) throws InstantiationException, IllegalAccessException, NoSuchMethodException,
            SecurityException, IllegalArgumentException, InvocationTargetException
    {
	final Object calc = c.newInstance ();
	final Method add = c.getMethod ("add", int.class, int.class);
	System.out.println ("2 + 2 = " + add.invoke (calc, 2, 2));
    }

    @DefineLisp
    public void tc (final Class<?> c, final int a, final int b) throws InstantiationException, IllegalAccessException,
            NoSuchMethodException, SecurityException, IllegalArgumentException, InvocationTargetException
    {
	final Object calc = c.newInstance ();
	final Method add = c.getMethod ("add", int.class, int.class);
	System.out.printf ("%s + %s = %s%n", a, b, add.invoke (calc, a, b));
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
