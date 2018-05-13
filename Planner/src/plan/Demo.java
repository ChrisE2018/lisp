
package plan;

import lisp.*;
import lisp.Package;
import lisp.demo.Repl;
import lisp.eval.Interpreter;

public class Demo
{
    public static void main (final String[] args)
    {
	final Demo d = new Demo ();
	d.processArguments (args);
	d.execute ();
    }

    private final Interpreter interpreter = new Interpreter ();
    private Package pkg = PackageFactory.getDefaultPackage ();
    private final FileReader fileReader = new FileReader ();
    private boolean doRepl = true;

    private Demo ()
    {
	PlanFunctions.initialize ();
    }

    private void processArguments (final String[] args)
    {
	for (int i = 1; i < args.length; i += 2)
	{
	    final String key = args[i - 1];
	    final String value = args[i];
	    try
	    {
		processArgument (key, value);
	    }
	    catch (final Exception e)
	    {
		e.printStackTrace ();
	    }
	}
    }

    private void processArgument (final String key, final String value) throws Exception
    {
	if (key.equals ("-p"))
	{
	    pkg = PackageFactory.getPackage (value);
	}
	if (key.equals ("-t"))
	{
	    fileReader.setTrace (Boolean.parseBoolean (value));
	}
	if (key.equals ("-f"))
	{
	    System.out.printf ("Loading %s %n", value);
	    fileReader.read (interpreter, pkg, value);
	}
	if (key.equals ("-r"))
	{
	    doRepl = Boolean.parseBoolean (value);
	}
    }

    private void execute ()
    {
	try
	{
	    if (doRepl)
	    {
		final Repl repl = new Repl (interpreter);
		final LispStream stream = new LispStream (System.in);
		repl.toplevel (stream);
	    }
	}
	catch (final Exception e)
	{
	    e.printStackTrace ();
	}
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
