
package lisp.gui;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.logging.*;

import lisp.*;
import lisp.Package;
import lisp.eval.*;
import lisp.util.*;

public class Application
{
    private static final Logger LOGGER = Logger.getLogger (Application.class.getName ());

    private final Map<String, String> parameterDocumentation = new HashMap<String, String> ();
    private final Map<String, ThrowingConsumer<String>> parameterActions = new HashMap<String, ThrowingConsumer<String>> ();

    private final Interpreter interpreter = new Interpreter ();

    public Application ()
    {
	// Try: defaults write -g NSAutomaticDashSubstitutionEnabled 0
	// --setq "var=form"
	// --eval "form"
	// --package pkg
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

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String expression) throws Exception
	    {
		final LispStream stream = new LispStream (expression);
		final LispReader lispReader = new LispReader ();
		final Object form = lispReader.read (stream);
		final Object result = interpreter.eval (new LexicalContext (interpreter), form);
		System.out.printf ("-E %s => %s %n", form, result);
	    }
	}, "Evaluate a form", "-E", "--eval");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String assignment) throws Exception
	    {
		final int pos = assignment.indexOf ("=");
		final String var = assignment.substring (0, pos);
		final String value = assignment.substring (pos + 1);
		final LispStream stream = new LispStream (value);
		final LispReader lispReader = new LispReader ();
		final Object form = lispReader.read (stream);
		final Object result = interpreter.eval (new LexicalContext (interpreter), form);
		final Package pkg = lispReader.getCurrentPackage ();
		final Symbol symbol = lispReader.readSymbol (pkg, var);
		symbol.setValue (result);
		System.out.printf ("-setq %s %s => %s %n", var, form, result);
	    }
	}, "Evaluate a form", "-S", "--setq");

	addParameter (new ThrowingConsumer<String> ()
	{
	    @Override
	    public void accept (final String packageName) throws Exception
	    {
		final Package pkg = PackageFactory.getPackage (packageName);
		PackageFactory.setDefaultPackage (pkg);
		final LispReader lispReader = LispReader.getLispThreadReader ();
		lispReader.setCurrentPackage (pkg);
		System.out.printf ("-Package %s %n", pkg);
	    }
	}, "Evaluate a form", "-P", "--package");
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
