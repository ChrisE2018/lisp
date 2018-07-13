
package plan;

import lisp.eval.*;
import lisp.gui.Repl;
import lisp.lang.*;
import lisp.lang.Package;

public class Demo
{
    public static void main (final String[] args)
    {
	try
	{
	    final Demo d = new Demo ();
	    d.processArguments (args);
	    d.execute ();
	}
	catch (Throwable e)
	{
	    while (e instanceof java.lang.reflect.InvocationTargetException)
	    {
		e = e.getCause ();
	    }
	    e.printStackTrace ();
	}
    }

    private final Interpreter interpreter = new Interpreter ();
    private Package pkg = PackageFactory.getDefaultPackage ();
    private final FileReader fileReader = new FileReader ();
    private boolean doRepl = true;

    private Demo ()
    {
	try
	{
	    final LexicalContext context = new LexicalContext (interpreter);
	    fileReader.read (context, "../../../lisp/src/lisp/eval/init.jisp");
	    PlanFunctions.initialize ();
	}
	catch (Throwable e)
	{
	    while (e instanceof java.lang.reflect.InvocationTargetException)
	    {
		e = e.getCause ();
	    }
	    e.printStackTrace ();
	}
    }

    private void processArguments (final String[] args) throws Exception
    {
	for (int i = 1; i < args.length; i += 2)
	{
	    final String key = args[i - 1];
	    final String value = args[i];
	    // try
	    // {
	    processArgument (key, value);
	    // }
	    // catch (final java.lang.reflect.InvocationTargetException e)
	    // {
	    // e.getCause ().printStackTrace ();
	    // }
	    // catch (final Exception e)
	    // {
	    // e.printStackTrace ();
	    // }
	}
    }

    private void processArgument (final String key, final String value) throws Exception
    {
	if (key.equals ("-p"))
	{
	    pkg = PackageFactory.getPackage (value);
	}
	if (key.equals ("-f"))
	{
	    System.out.printf ("Loading %s %n", value);
	    final LexicalContext context = new LexicalContext (interpreter);
	    fileReader.read (context, value);
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
		final LispStream stream = new LispInputStream (System.in);
		repl.toplevel (stream);
	    }
	}
	catch (Throwable e)
	{
	    while (e instanceof java.lang.reflect.InvocationTargetException)
	    {
		e = e.getCause ();
	    }
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
