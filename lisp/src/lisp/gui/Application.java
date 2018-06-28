
package lisp.gui;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.logging.*;

import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.*;

public class Application
{
    private static final Logger LOGGER = Logger.getLogger (Application.class.getName ());

    private final Map<String, String> parameterDocumentation = new HashMap<String, String> ();
    private final Map<String, ThrowingConsumer<String>> parameterActions = new HashMap<String, ThrowingConsumer<String>> ();

    public Application (final Interpreter interpreter)
    {
	final LispReader lispReader = LispReader.getLispThreadReader ();
	// Try: defaults write -g NSAutomaticDashSubstitutionEnabled 0
	// --eval "form"
	// --exit "message"
	// --load "pathname"
	// --log "config"
	// --package pkg
	// --setq "var=form"

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String expression) throws Exception
	    {
		final LispStream stream = new LispInputStream (expression);
		// final LispReader lispReader = new LispReader ();
		final Object form = lispReader.read (stream);
		final Object result = interpreter.eval (new LexicalContext (interpreter), form);
		LOGGER.fine (new LogString ("-E %s => %s", form, result));
	    }
	}, "Evaluate a form", "-E", "--eval");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String message) throws Exception
	    {
		LOGGER.fine ("--exit " + message);
		Repl.exit ();
	    }
	}, "Exit Lisp system", "--exit");

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

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String filename) throws SecurityException, IOException
	    {
		final LogManager logManager = LogManager.getLogManager ();
		final URL resource = getClass ().getResource (filename);
		logManager.readConfiguration (resource.openStream ());
		LOGGER.fine ("Logging Configuration: " + resource);
	    }
	}, "Load logging properties file", "-g", "--log");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String packageName) throws Exception
	    {
		final Package pkg = PackageFactory.getPackage (packageName);
		lispReader.setCurrentPackage (pkg);
		LOGGER.fine (new LogString ("-Package %s", pkg));
	    }
	}, "Evaluate a form", "-P", "--package");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String assignment) throws Exception
	    {
		final int pos = assignment.indexOf ("=");
		final String var = assignment.substring (0, pos);
		final String value = assignment.substring (pos + 1);
		final LispStream stream = new LispInputStream (value);
		// final LispReader lispReader = new LispReader ();
		final Object form = lispReader.read (stream);
		final Object result = interpreter.eval (new LexicalContext (interpreter), form);
		final Package pkg = lispReader.getCurrentPackage ();
		final Symbol symbol = lispReader.readSymbol (pkg, var);
		symbol.setValue (result);
		LOGGER.fine (new LogString ("-setq %s %s => %s", var, form, result));
	    }
	}, "Evaluate a form", "-S", "--setq");
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
	    if (action != null)
	    {
		action.accept (value);
	    }
	    else
	    {
		LOGGER.severe ("Invalid application parameter " + key);
		throw new Error ("Invalid application parameter " + key);
	    }
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
