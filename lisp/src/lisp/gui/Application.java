
package lisp.gui;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.logging.*;

import lisp.eval.Interpreter;
import lisp.util.*;

public class Application
{
    private static final Logger LOGGER = Logger.getLogger (Application.class.getName ());

    private final Map<String, String> parameterDocumentation = new HashMap<String, String> ();
    private final Map<String, ThrowingConsumer<String>> parameterActions = new HashMap<String, ThrowingConsumer<String>> ();

    private final Interpreter interpreter = new Interpreter ();

    public Application ()
    {
	// [TODO] --setq "var=form"
	// [TODO] --package pkg
	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String filename) throws SecurityException, IOException
	    {
		final LogManager logManager = LogManager.getLogManager ();
		final URL resource = getClass ().getResource (filename);
		logManager.readConfiguration (resource.openStream ());
		LOGGER.info ("Logging Configuration: " + resource);
	    }
	}, "Load logging properties file", "-g", "--log");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String filename) throws Exception
	    {
		if (!interpreter.loadResource (filename))
		{
		    interpreter.loadFile (filename);
		}
	    }
	}, "Load lisp resource or file", "-l", "--load");
    }

    public void addParameter (final ThrowingConsumer<String> action, final String doc, final String... keys)
    {
	for (final String key : keys)
	{
	    parameterDocumentation.put (key, doc);
	    parameterActions.put (key, action);
	}
    }

    public void initialize (final String[] args) throws Exception
    {
	for (int i = 1; i < args.length; i += 2)
	{
	    final String key = args[i - 1];
	    final String value = args[i];
	    LOGGER.finer (new LogString ("Initialize %s: %s", key, value));
	    final ThrowingConsumer<String> action = parameterActions.get (key);
	    action.accept (value);
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
