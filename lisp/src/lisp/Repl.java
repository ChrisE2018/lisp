
package lisp;

public class Repl
{
    private final Package pkg = PackageFactory.getSystemPackage ();

    private final Reader reader = new Reader ();

    private final Interpreter interpreter;

    public static void main (final String[] args) throws NoSuchMethodException, SecurityException
    {
	final Repl repl = new Repl ();
	repl.repl ();
    }

    @SuppressWarnings ("unused")
    private Repl () throws NoSuchMethodException, SecurityException
    {
	new Primitives ();
	interpreter = new Interpreter ();
    }

    private void repl ()
    {
	final LispStream stream = new LispStream (System.in);
	while (true)
	{
	    try
	    {
		Thread.sleep (100);
		rep (stream);
	    }
	    catch (final Throwable e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    private void rep (final LispStream stream) throws Exception
    {
	final Lisp form = reader.read (stream, pkg);
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	final StringBuilder buffer = new StringBuilder ();
	form.print (buffer);
	System.out.println (buffer);
	System.out.println (form);
	System.out.print (" ==> ");
	final Lisp value = interpreter.eval (form);
	buffer.setLength (0);
	if (value == null)
	{
	    buffer.append ("null");
	}
	else
	{
	    value.print (buffer);
	}
	System.out.println (buffer);
	System.out.println (value);
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
