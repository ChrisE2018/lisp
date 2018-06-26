
package lisp.util;

public class Assignment
{
    private static Class<?>[] CLASSES =
        {boolean.class, char.class, byte.class, short.class, int.class, long.class, float.class, double.class};

    public static void main (final String[] args)
    {
	final Assignment a = new Assignment ();
	a.execute ();
    }

    private void execute ()
    {
	System.out.printf ("%10s%10s%10s%10s%n", "", "", "", "FROM");
	System.out.printf ("%10s", "TO");
	for (int j = 0; j < CLASSES.length; j++)
	{
	    final Class<?> from = CLASSES[j];
	    System.out.printf ("%10s", from.getSimpleName ());
	}
	System.out.println ();
	for (int i = 0; i < CLASSES.length; i++)
	{
	    final Class<?> to = CLASSES[i];
	    final StringBuilder buffer = new StringBuilder ();
	    buffer.append (String.format ("%10s", to.getSimpleName ()));
	    for (int j = 0; j < CLASSES.length; j++)
	    {
		final Class<?> from = CLASSES[j];
		if (to.isAssignableFrom (from))
		{
		    buffer.append (String.format ("%10s", "yes"));
		}
		else
		{
		    buffer.append (String.format ("%10s", "no"));
		}
	    }
	    System.out.println (buffer);
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
