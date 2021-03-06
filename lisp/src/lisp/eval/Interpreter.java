
package lisp.eval;

import java.io.*;
import java.lang.reflect.*;
import java.net.URL;
import java.util.List;
import java.util.logging.*;

import lisp.lang.*;
import lisp.lang.FileReader;
import lisp.symbol.*;

/**
 * Simple interpreter that uses reflection to evaluate forms like Lisp functions. Everything
 * required to process the init file is defined here in code. <br/>
 * The init file is loaded and defines everything else.
 */
public class Interpreter extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (Interpreter.class.getName ());
    private static final Applicable applicable = new Applicable ();

    private static Invoke invoke = new Invoke ();

    public Interpreter ()
    {
    }

    public Object eval (final LexicalContext context, final Object form) throws Exception
    {
	if (form instanceof Symbol)
	{
	    final Object result = context.get ((Symbol)form);
	    return result;
	}
	if (form instanceof List<?>)
	{
	    final List<? extends Object> list = (List<?>)form;
	    if (list.size () == 0)
	    {
		return form;
	    }
	    final Object fn = list.get (0);
	    if (fn instanceof java.lang.reflect.Method)
	    {
		// (java.lang.System.getenv "USER")
		final java.lang.reflect.Method method = (java.lang.reflect.Method)fn;
		if (Modifier.isStatic (method.getModifiers ()))
		{
		    // If the method comes from the Lisp reader it may not be the correct overload,
		    // so we ignore the method and use the name to select one that fits the actual
		    // parameters.
		    final List<Object> arguments = new LispList ();
		    for (int i = 1; i < list.size (); i++)
		    {
			arguments.add (eval (context, list.get (i)));
		    }
		    return invoke.javaMethodCall (null, method.getDeclaringClass (), method.getName (), arguments);
		}
		else
		{
		    // (java.lang.String.length "foobar")
		    final List<Object> arguments = new LispList ();
		    final Object target = eval (context, list.get (1));
		    for (int i = 2; i < list.size (); i++)
		    {
			arguments.add (eval (context, list.get (i)));
		    }
		    return invoke.javaMethodCall (target, method.getDeclaringClass (), method.getName (), arguments);
		}
	    }
	    if (fn instanceof List)
	    {
		final List<?> dotForm = (List<?>)fn;
		// Handle fn like (dot (field class java.lang.System "out") "println")
		final Object f = dotForm.get (0);
		if (f instanceof Symbol)
		{
		    final Symbol fs = (Symbol)f;
		    if (fs.is ("dot"))
		    {
			// (System.out.println "foo")
			// (String.format "foo")
			final String methodName = (String)dotForm.get (2);
			final Object target = eval (context, dotForm.get (1));
			final List<Object> arguments = new LispList ();
			for (int i = 1; i < list.size (); i++)
			{
			    arguments.add (eval (context, list.get (i)));
			}
			if (target instanceof Class)
			{
			    return invoke.javaMethodCall (null, (Class<?>)target, methodName, arguments);
			}
			else
			{
			    return invoke.javaMethodCall (target, target.getClass (), methodName, arguments);
			}
		    }
		}
		throw new Error ("NYI fn as " + fn);
	    }
	    if (fn instanceof Symbol)
	    {
		final Symbol f = (Symbol)fn;
		final FunctionCell function = f.getFunction ();
		if (function != null)
		{
		    return function.eval (context, list);
		}
		else if (list.size () > 1)
		{
		    // Handle unbound functions as calls to native Java methods
		    // (length "foobar")
		    final Object target = eval (context, list.get (1));
		    final String method = coerceString (f, true);
		    if (target == null)
		    {
			throw new NullPointerException ("Can't apply " + method + " to null");
		    }
		    final Class<?> cls = target.getClass ();
		    final List<Object> arguments = new LispList ();
		    for (int i = 2; i < list.size (); i++)
		    {
			arguments.add (eval (context, list.get (i)));
		    }
		    return invoke.javaMethodCall (target, cls, method, arguments);
		}
		else
		{
		    throw new IllegalArgumentException ("Undefined function " + fn);
		}
	    }
	    else
	    {
		throw new IllegalArgumentException ("Function is not a symbol " + fn);
	    }
	}
	return form;
    }

    /**
     * Evaluator for quoted forms.
     *
     * @param context Required by calling convention but not used.
     * @param interpreter Not used, but required by calling protocol.
     */
    @DefineLisp (special = true, name = "quote", classname = "lisp.special.QuoteFunction")
    public Object quoteEvaluator (final LexicalContext context, final Object result)
    {
	return result;
    }

    /** Evaluate a lisp expression and return the result. */
    @DefineLisp (name = "eval")
    public Object evalT (final Object expression) throws Exception
    {
	final LexicalContext context = new LexicalContext (this);
	final Object value = eval (context, expression);
	return value;
    }

    /** Return a class for a name. This is required so the init file can load the primitives. */
    @DefineLisp
    public Object classForName (final String className) throws ClassNotFoundException
    {
	final Class<?> c = Class.forName (className);
	return c;
    }

    @DefineLisp (name = "new")
    public Object newFunction (final String className, final Object... args)
            throws ClassNotFoundException, InstantiationException, IllegalAccessException
    {
	final Class<?> cls = Class.forName (className);
	return newFunction (cls, args);
    }

    @DefineLisp (name = "new")
    public Object newFunction (final Class<?> cls, final Object... args) throws InstantiationException, IllegalAccessException
    {
	final int n = args.length;
	if (n > 0)
	{
	    final Constructor<?>[] constructors = cls.getDeclaredConstructors ();
	    for (final Constructor<?> constructor : constructors)
	    {
		try
		{
		    // Should do full overload selection here
		    if (applicable.applicable (constructor, args))
		    {
			// if (constructor.isVarArgs ())
			// {
			// if (constructor.getParameterCount () - 1 <= n)
			// {
			// final Object result = constructor.newInstance (args);
			// return result;
			// }
			// }
			// else if (constructor.getParameterCount () == n)
			{
			    final Object result = constructor.newInstance (args);
			    return result;
			}
		    }
		}
		catch (final InstantiationException | IllegalArgumentException | InvocationTargetException
		        | IllegalAccessException x)
		{
		    if (LOGGER.isLoggable (Level.FINEST))
		    {
			LOGGER.log (Level.FINEST, "Can't instantiate " + cls + " : ", x);
		    }
		}
	    }
	    throw new InstantiationException ("No " + cls + " constructor found for " + args);
	}
	else
	{
	    return cls.newInstance ();
	}
    }

    /**
     * Test case: (static "java.lang.System" "exit" 1)
     *
     * @throws NoSuchMethodException
     */
    @DefineLisp (name = "static")
    public Object staticClass (final String className, final String methodName, final Object... args)
            throws ClassNotFoundException, NoSuchMethodException
    {
	final Class<?> cls = Class.forName (className);

	final Method[] methods = cls.getDeclaredMethods ();
	for (final Method m : methods)
	{
	    try
	    {
		if (Modifier.isStatic (m.getModifiers ()) && m.getName ().equals (methodName))
		{
		    final Object result = m.invoke (cls, args);
		    return result;
		}
	    }
	    catch (IllegalArgumentException | InvocationTargetException | IllegalAccessException ex)
	    {
		ex.printStackTrace ();
	    }
	}
	throw new NoSuchMethodException ("Method " + className + "." + methodName + " not found");
    }

    // public Object evalString (final String expression) throws Exception
    // {
    // final LexicalContext context = new LexicalContext (this);
    // final LispStream stream = new LispInputStream (expression);
    // final LispReader lispReader = new LispReader ();
    // final Object form = lispReader.read (stream);
    // return eval (context, form);
    // }

    @DefineLisp
    public boolean loadResource (final String pathname) throws Exception
    {
	final URL url = Interpreter.class.getResource (pathname);
	if (url != null)
	{
	    final FileReader fileReader = new FileReader ();
	    try
	    {
		System.out.printf ("Loading resource file %s %n", url);
		final LexicalContext context = new LexicalContext (this);
		fileReader.read (context, url);
		return true;
	    }
	    catch (final EOFException e)
	    {
		System.out.printf ("EOF loading resource file%n", url);
		return true;
	    }
	}
	return false;
    }

    /** Load a file. First argument is the pathname. */
    @DefineLisp
    public boolean loadFile (final String pathname) throws Exception
    {
	final File file = new File (pathname);
	if (file.canRead ())
	{
	    try
	    {
		System.out.printf ("Loading system file %s %n", pathname);
		final LexicalContext context = new LexicalContext (this);
		final FileReader fileReader = new FileReader ();
		fileReader.read (context, file);
	    }
	    catch (final EOFException e)
	    {
		System.out.printf ("EOF reading system file%n", pathname);
		return true;
	    }
	}
	return false;
    }

    @DefineLisp (name = "throw")
    public void throwFunction (final Throwable exception) throws Throwable
    {
	throw exception;
    }

    @DefineLisp
    public void throwError (final String format, final Object... args) throws Throwable
    {
	final String message = String.format (format, args);
	throw new Error (message);
    }

    @DefineLisp
    public void throwWarning (final String format, final Object... args) throws Throwable
    {
	final String message = String.format (format, args);
	throw new Throwable (message);
    }

    /**
     * Try-catch implementation. (try <protectedForm> <catchClause>*) Where <catchClause> ::=
     * (<exceptionClass> <ExceptionVariable> <expression>*)
     *
     * @param interpreter
     * @param form
     * @param catchClauses
     * @return
     * @throws Exception
     */
    @DefineLisp (name = "try", special = true)
    public Object tryCatch (final LexicalContext context, final Object form, final Object... catchClauses) throws Exception
    {
	// FIXME Implement TryCatch compiler.
	Object result = null;
	try
	{
	    result = context.eval (form);
	}
	catch (final Throwable e)
	{
	    final Class<?> eclass = e.getClass ();
	    for (final Object clause : catchClauses)
	    {
		@SuppressWarnings ("unchecked")
		final List<Object> c = (List<Object>)clause;
		final String className = (String)c.get (0);
		final Class<?> cls = Class.forName (className);
		if (cls.isAssignableFrom (eclass))
		{
		    final Symbol var = (Symbol)c.get (1);
		    final Object oldValue = var.getValue ();
		    try
		    {
			var.setValue (e);
			for (int i = 2; i < c.size (); i++)
			{
			    result = context.eval (c.get (i));
			}
		    }
		    finally
		    {
			var.setValue (oldValue);
		    }
		    return result;
		}
	    }
	    // Didn't actually want to catch it.
	    throw e;
	}
	return result;
    }
}
