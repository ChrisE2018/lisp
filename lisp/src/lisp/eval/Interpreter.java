
package lisp.eval;

import java.io.*;
import java.lang.reflect.*;
import java.net.URL;
import java.util.List;

import lisp.*;
import lisp.FileReader;
import lisp.Package;
import lisp.symbol.FunctionCell;

/**
 * Simple interpreter that uses reflection to evaluate forms like Lisp functions. Everything
 * required to process the init file is defined here in code. <br/>
 * The init file is loaded and defines everything else.
 */
public class Interpreter extends Definer
{
    // private static final Logger LOGGER = Logger.getLogger (Interpreter.class.getName ());

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
		// Handle fn like (dot object method)
		// (java.lang.System.out.println)
		// (java.lang.System.out.print "This is output")
		// (java.lang.System.out.println "This is output")
		// (java.lang.System.out.printf "foo %s %n" 172)
		final List<Object> arguments = new LispList ();
		final Object target = dotForm.get (1);
		final Method method = (Method)dotForm.get (2);
		for (int i = 1; i < list.size (); i++)
		{
		    arguments.add (eval (context, list.get (i)));
		}
		return invoke.javaMethodCall (target, method.getDeclaringClass (), method.getName (), arguments);
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
    public Object ClassForName (final String className) throws ClassNotFoundException
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
	// if (args.length > 0)
	// {
	// final Constructor<?>[] constructors = cls.getDeclaredConstructors ();
	// for (final Constructor<?> c : constructors)
	// {
	// try
	// {
	// final Object result = c.newInstance (args);
	// return result;
	// }
	// catch (final InstantiationException | IllegalArgumentException |
	// InvocationTargetException
	// | IllegalAccessException x)
	// {
	// }
	// }
	// }
	// return cls.newInstance ();
    }

    @DefineLisp (name = "new")
    public Object newFunction (final Class<?> cls, final Object... args) throws InstantiationException, IllegalAccessException
    {
	if (args.length > 0)
	{
	    final Constructor<?>[] constructors = cls.getDeclaredConstructors ();
	    for (final Constructor<?> c : constructors)
	    {
		try
		{
		    final Object result = c.newInstance (args);
		    return result;
		}
		catch (final InstantiationException | IllegalArgumentException | InvocationTargetException
		        | IllegalAccessException x)
		{
		}
	    }
	}
	return cls.newInstance ();
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

    public Object evalString (final String expression) throws Exception
    {
	final LexicalContext context = new LexicalContext (this);
	final LispStream stream = new LispStream (expression);
	final LispReader lispReader = new LispReader ();
	final Object form = lispReader.read (stream);
	return eval (context, form);
    }

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
		final Package pkg = PackageFactory.getDefaultPackage ();
		final LexicalContext context = new LexicalContext (this);
		final FileReader fileReader = new FileReader ();
		fileReader.read (context, pkg, file);
	    }
	    catch (final EOFException e)
	    {
		System.out.printf ("EOF reading system file%n", pathname);
		return true;
	    }
	}
	return false;
    }

    /** Load a file. First argument is the pathname. Optional second argument is the package. */
    @DefineLisp
    public Object loadFile (final String pathname, final Object p) throws Exception
    {
	final Package pkg = coercePackage (p);
	final LexicalContext context = new LexicalContext (this);
	final FileReader fileReader = new FileReader ();
	final Object result = fileReader.read (context, pkg, pathname);
	return result;
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
