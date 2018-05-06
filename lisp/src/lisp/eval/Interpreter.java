
package lisp.eval;

import java.lang.reflect.*;
import java.util.List;

import lisp.*;
import lisp.Package;

/** Simple interpreter that using reflection to evaluate forms like Lisp functions. */
public class Interpreter extends BasicDefiner
{
    public Interpreter () throws NoSuchMethodException, SecurityException
    {
	final Symbol eval = PackageFactory.getSystemPackage ().internPublic ("eval");
	final Symbol load = PackageFactory.getSystemPackage ().internPublic ("load");
	final Symbol java = PackageFactory.getSystemPackage ().internPublic ("java");
	define (eval, "evalEvaluator");
	define (load, "loadEvaluator");
	define (java, "javaEvaluator");
	// [TODO] Need javaStatic, javaNew
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
	    final List<Object> list = (List<Object>)form;
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
	    if (function == null)
	    {
		// [TODO] If function is bound to a java object, try to form a method call
		// throw new IllegalArgumentException ("Symbol has no function definition " + f);
		final Object target = eval (getObject (list, 1));
		final String method = coerceString (f, true);
		final Class<?> cls = target.getClass ();
		final List<Object> arguments = new LispList ();
		arguments.add (target);
		arguments.add (method);
		for (int i = 2; i < list.size (); i++)
		{
		    arguments.add (eval (list.get (i)));
		}
		// System.out.printf ("Invoking %s.%s %s %n", target, method, arguments);
		return javaMethodCall (target, cls, method, arguments);
	    }
	    // System.out.printf ("Eval %s%n", form);
	    final Object result = function.eval (this, list);
	    return result;
	}
	return form;
    }

    /** Evaluate a lisp expression and return the result. */
    public Object evalEvaluator (final List<Object> arguments) throws Exception
    {
	final Object expression = arguments.get (0);
	final Object value = eval (expression);
	return value;
    }

    /** Load a file. First argument is the pathname. Optional second argument is the package. */
    public Object loadEvaluator (final List<Object> arguments) throws Exception
    {
	final String pathname = coerceString (arguments.get (0));
	Package pkg = PackageFactory.getDefaultPackage ();
	if (arguments.size () > 1)
	{
	    pkg = coercePackage (arguments.get (1));
	}
	final FileReader fileReader = new FileReader ();
	final Object result = fileReader.read (this, pkg, pathname);
	return result;
    }

    public Object javaEvaluator (final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object target = getObject (arguments, 0);
	final String method = coerceString (arguments.get (1), true);
	final Class<?> cls = target.getClass ();
	return javaMethodCall (target, cls, method, arguments);
    }

    /**
     * Recursive method to perform a java method call.Actual arguments start at argument 2.
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
	    throw new IllegalArgumentException ("Can't apply method " + methodName + " to object " + target);
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
	    final Object arg = arguments.get (i + 2);
	    final Object actual = coerceToParameter (parameters[i], arg);
	    actuals[i] = actual;
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
	}
	return method.invoke (target, actuals);
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
