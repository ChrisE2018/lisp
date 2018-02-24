
package lisp;

import java.io.IOException;

public class Rpl
{
    private final Reader reader = new Reader ();

    public static void main (final String[] args)
    {
	final Rpl rpl = new Rpl ();
	rpl.rpl ();
    }

    private void rpl ()
    {
	while (true)
	{
	    try
	    {
		Thread.sleep (100);
		rp ();
	    }
	    catch (final Throwable e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    private void rp () throws IOException
    {
	final Lisp form = reader.read (System.in);
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	final StringBuilder buffer = new StringBuilder ();
	form.print (buffer);
	System.out.println (buffer);
	System.out.println (form);
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
