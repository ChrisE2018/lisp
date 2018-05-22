
package lisp.demo;

import lisp.*;
import lisp.Package;

/** Simple toplevel loop that reads a lisp form and prints it back. */
public class Rpl
{
    private final Package pkg = PackageFactory.getSystemPackage ();

    private final LispReader reader = new LispReader ();

    public static void main (final String[] args)
    {
	final Rpl rpl = new Rpl ();
	rpl.rpl ();
    }

    private void rpl ()
    {
	final LispStream stream = new LispStream (System.in);
	while (true)
	{
	    try
	    {
		Thread.sleep (100);
		rp (stream);
	    }
	    catch (final Throwable e)
	    {
		e.printStackTrace ();
		try
		{
		    // Read to a newline character
		    while (stream.read () != '\n')
		    {

		    }
		}
		catch (final Throwable ex)
		{
		    System.out.printf ("[Error recovering from error: %s]", ex);
		}
	    }
	}
    }

    private void rp (final LispStream stream) throws Exception
    {
	final Object form = reader.read (stream, pkg);
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	final StringBuilder buffer = new StringBuilder ();
	LispReader.printElement (buffer, form);
	System.out.println (buffer);
	System.out.println (form);
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
