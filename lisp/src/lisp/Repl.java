
package lisp;

public class Repl
{
    // private final Package pkg = PackageFactory.getSystemPackage ();

    private final Reader reader = new Reader ();

    private final Interpreter interpreter;

    public static void main (final String[] args) throws NoSuchMethodException, SecurityException
    {
	final Repl repl = new Repl ();
	repl.toplevel ();
    }

    /** Constructor for demo application. */
    private Repl () throws NoSuchMethodException, SecurityException
    {
	Primitives.initialize ();
	interpreter = new Interpreter ();
    }

    /** Constructor to use an interpreter built elsewhere. */
    public Repl (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
    }

    public void toplevel ()
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
	final Package pkg = PackageFactory.getDefaultPackage ();
	final Object form = reader.read (stream, pkg);
	if (form == null)
	{
	    System.out.println ("Exit");
	    return;
	}
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (form.toString ());
	System.out.println (buffer);
	System.out.println (form);
	System.out.print (" ==> ");
	final Object value = interpreter.eval (form);
	buffer.setLength (0);
	if (value == null)
	{
	    buffer.append ("null");
	}
	else
	{
	    buffer.append (value.toString ());
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
