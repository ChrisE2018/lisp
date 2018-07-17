
package plan.gui;

import java.util.logging.*;

import lisp.eval.*;
import lisp.gui.Repl;
import lisp.lang.*;
import lisp.util.LogString;
import plan.PlanFunctions;

public class Demo
{
    private static final LogManager logManager = LogManager.getLogManager ();
    private static final Logger LOGGER = Logger.getLogger (Demo.class.getName ());

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
	    LOGGER.log (Level.WARNING, "Startup Error", e);
	}
    }

    private final Interpreter interpreter = new Interpreter ();
    private final FileReader fileReader = new FileReader ();
    private boolean doRepl = true;

    private Demo ()
    {
	try
	{
	    logManager.readConfiguration (Demo.class.getResource ("logging.properties").openStream ());
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
	    LOGGER.log (Level.WARNING, "Initialization Error", e);
	}
    }

    private void processArguments (final String[] args) throws Exception
    {
	for (int i = 1; i < args.length; i += 2)
	{
	    final String key = args[i - 1];
	    final String value = args[i];
	    processArgument (key, value);
	}
    }

    private void processArgument (final String key, final String value) throws Exception
    {
	if (key.equals ("-f"))
	{
	    LOGGER.info (new LogString ("Loading %s", value));
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
	    LOGGER.log (Level.WARNING, "Repl Error", e);
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
