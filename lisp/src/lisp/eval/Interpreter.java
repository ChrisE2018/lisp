
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
    public Interpreter ()
    {
    }

    public Object eval (final Object form) throws Exception
    {
	if (form instanceof Symbol)
	{
	    final Symbol symbol = (Symbol)form;
	    final Object result = symbol.getValue ();
	    return result;
	}
	if (form instanceof List<?>)
	{
	    final List<?> list = (List<?>)form;
	    if (list.size () == 0)
	    {
		return form;
	    }
	    final Object fn = list.get (0);
	    if (!(fn instanceof Symbol))
	    {
		throw new IllegalArgumentException ("Function is not a symbol " + fn);
	    }
	    final Symbol f = (Symbol)fn;
	    final FunctionCell function = f.getFunction ();
	    if (function != null)
	    {
		return function.eval (this, list);
	    }
	    else if (list.size () > 1)
	    {
		// Handle unbound functions as calls to native Java methods
		final Object target = eval (list.get (1));
		final String method = coerceString (f, true);
		if (target == null)
		{
		    throw new NullPointerException ("Can't apply " + method + " to null");
		}
		final Class<?> cls = target.getClass ();
		final List<Object> arguments = new LispList ();
		arguments.add (target);
		arguments.add (method);
		for (int i = 2; i < list.size (); i++)
		{
		    arguments.add (eval (list.get (i)));
		}
		return javaMethodCall (target, cls, method, arguments);
	    }
	    else
	    {
		throw new IllegalArgumentException ("Undefined function " + fn);
	    }
	}
	return form;
    }

    /**
     * Recursive method to perform a java method call. Actual arguments start at argument 2.
     *
     * @throws InvocationTargetException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    private Object javaMethodCall (final Object target, final Class<?> cls, final String methodName, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	// Get local methods. Need to apply recursively to superclass
	final Method[] methods = cls.getDeclaredMethods ();
	for (final Method method : methods)
	{
	    if (method.getName ().equals (methodName))
	    {
		if (method.getParameterCount () == arguments.size () - 2)
		{
		    try
		    {
			return invokeMethod (target, method, arguments);
		    }
		    catch (final CoerceError e)
		    {

		    }
		}
	    }
	}
	final Class<?> parentClass = cls.getSuperclass ();
	if (parentClass == null)
	{
	    throw new IllegalArgumentException ("Can't apply method '" + methodName + "' to object " + target);
	}
	return javaMethodCall (target, parentClass, methodName, arguments);
    }

    private Object invokeMethod (final Object target, final Method method, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] actuals = new Object[parameters.length];
	for (int i = 0; i < parameters.length; i++)
	{
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
	    final Object arg = arguments.get (i + 2);
	    final Object actual = coerceToParameter (parameters[i], arg);
	    actuals[i] = actual;
	}
	return method.invoke (target, actuals);
    }

    private Object coerceToParameter (final Class<?> p, final Object arg)
    {
	final Class<?> argClass = arg.getClass ();
	if (p.isAssignableFrom (argClass))
	{
	    return arg;
	}
	if (p == String.class)
	{
	    // Handle String from Symbol or String
	    if (arg instanceof Symbol)
	    {
		return ((Symbol)arg).getName ();
	    }
	    if (arg instanceof String)
	    {
		return arg;
	    }
	}
	// [TODO] Handle char, long, short etc.
	if (p == int.class || p == Integer.class)
	{
	    // Handle int from Lisp int
	    if (arg instanceof Integer)
	    {
		return arg;
	    }
	}
	if (p == double.class || p == Double.class)
	{
	    if (arg instanceof Double)
	    {
		return arg;
	    }
	}
	if (p == boolean.class || p == Boolean.class)
	{
	    if (arg instanceof Boolean)
	    {
		return arg;
	    }
	}
	throw new CoerceError ("Can't coerce %s to %s", arg, p);
    }

    /**
     * Evaluator for quoted forms.
     *
     * @param interpreter Not used, but required by calling protocol.
     */
    @DefineLisp (special = true, name = "quote")
    public Object quoteEvaluator (final Interpreter interpreter, final Object result)
    {
	return result;
    }

    /** Evaluate a lisp expression and return the result. */
    @DefineLisp (name = "eval")
    public Object evalT (final Object expression) throws Exception
    {
	final Object value = eval (expression);
	return value;
    }

    @DefineLisp (special = true)
    public Interpreter getInterpreter (final Interpreter result)
    {
	return result;
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
		fileReader.read (this, url);
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
		final FileReader fileReader = new FileReader ();
		fileReader.read (this, pkg, file);
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
	final FileReader fileReader = new FileReader ();
	final Object result = fileReader.read (this, pkg, pathname);
	return result;
    }

    @DefineLisp (name = "throw")
    public void throwFunction (final Throwable exception) throws Throwable
    {
	throw exception;
    }

    @DefineLisp
    public void error (final String format, final Object... args) throws Throwable
    {
	final String message = String.format (format, args);
	throw new Error (message);
    }

    @DefineLisp
    public void warning (final String format, final Object... args) throws Throwable
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
    public Object tryCatch (final Interpreter interpreter, final Object form, final Object... catchClauses) throws Exception
    {
	Object result = null;
	try
	{
	    result = interpreter.eval (form);
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
			    result = interpreter.eval (c.get (i));
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
	buffer.append (">");
	return buffer.toString ();
    }
}
