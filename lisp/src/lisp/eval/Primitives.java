
package lisp.eval;

import java.lang.reflect.*;
import java.util.*;

import lisp.*;
import lisp.Package;

public class Primitives extends Definer
{
    public static void initialize () throws NoSuchMethodException, SecurityException
    {
	final Primitives primitives = new Primitives ();
	primitives.definePrimitives ();
    }

    private Primitives ()
    {
	super (PackageFactory.getSystemPackage ());
    }

    private void definePrimitives () throws NoSuchMethodException, SecurityException
    {
	defspecial ("quote", "quoteEvaluator");
	defspecial ("def", "defEvaluator");
	defspecial ("setq", "setqEvaluator");
	define ("list", "listEvaluator");
	define ("plus", "plusEvaluator");
	define ("+", "plusEvaluator");
	define ("times", "timesEvaluator");
	define ("*", "timesEvaluator");
	define ("not", "notEvaluator");
	define ("in-package", "inPackageEvaluator");
	define ("java", "javaEvaluator");
	// [TODO] Need javaStatic, javaNew
	define ("describe", "describeEvaluator");
	define ("getDefaultPackage", "getDefaultPackageEvaluator");
	define ("getSystemPackage", "getSystemPackageEvaluator");
	define ("getParentPackage", "getParentPackageEvaluator");
	define ("getChildPackages", "getChildPackagesEvaluator");
    }

    /**
     * Evaluator for quoted forms.
     *
     * @param interpreter Not used, but required by calling protocol.
     */
    public Object quoteEvaluator (final Interpreter interpreter, final List<Object> arguments)
    {
	final Object result = arguments.get (1);
	return result;
    }

    /**
     * Interpreter for function definitions.
     *
     * @param interpreter Not used, but required by calling protocol.
     */
    public Object defEvaluator (final Interpreter interpreter, final List<Object> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (1), true);
	final LispList arglist = (LispList)arguments.get (2);
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Object a : arglist)
	{
	    params.add ((Symbol)a);
	}
	final List<Object> body = new ArrayList<Object> ();
	for (int i = 3; i < arguments.size (); i++)
	{
	    body.add (arguments.get (i));
	}
	final DefFunctionCell function = new DefFunctionCell (name, params, body);
	name.setFunction (function);
	return name;
    }

    /**
     * Interpreter for setq statements.
     *
     * @param interpreter The interpreter used to evaluate forms.
     * @param arguments The symbol and value form.
     * @return The new value.
     */
    public Object setqEvaluator (final Interpreter interpreter, final List<Object> arguments) throws Exception
    {
	final Symbol name = coerceSymbol (arguments.get (1), true);
	final Object form = arguments.get (2);
	final Object value = interpreter.eval (form);
	name.setValue (value);
	return value;
    }

    public Object listEvaluator (final List<Object> arguments)
    {
	return new LispList (arguments);
    }

    public Object plusEvaluator (final List<Object> arguments)
    {
	int result = 0;
	double dresult = 0;
	boolean integer = true;
	for (final Object a : arguments)
	{
	    if (a instanceof Integer)
	    {
		result += (Integer)a;
	    }
	    else if (a instanceof Double)
	    {
		integer = false;
		dresult += (Double)a;
	    }
	    else
	    {
		throw new IllegalArgumentException ("Number required " + a);
	    }
	}
	if (integer)
	{
	    return new Integer (result);
	}
	return new Double (result + dresult);
    }

    public Object timesEvaluator (final List<Object> arguments)
    {
	int result = 1;
	double dresult = 1;
	boolean integer = true;
	for (final Object a : arguments)
	{
	    if (a instanceof Integer)
	    {
		result *= (Integer)a;
	    }
	    else if (a instanceof Double)
	    {
		integer = false;
		dresult *= (Double)a;
	    }
	    else
	    {
		throw new IllegalArgumentException ("Number required " + a);
	    }
	}
	if (integer)
	{
	    return new Integer (result);
	}
	return new Double (result * dresult);
    }

    public Object notEvaluator (final List<Object> arguments)
    {
	if (arguments.size () != 1)
	{
	    throw new IllegalArgumentException ("Exactly one argument required");
	}
	final Object arg = arguments.get (0);
	if (arg instanceof Boolean)
	{
	    final Boolean b = (Boolean)arg;
	    return !b;
	}
	return Boolean.FALSE;
    }

    public Object inPackageEvaluator (final List<Object> arguments)
    {
	final Package pkg = coercePackage (arguments.get (0), true);
	PackageFactory.setDefaultPackage (pkg);
	return pkg;
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
     * Value to return when a method call fails. <br>
     * [TODO] Use an Exception instead.
     */
    private static final Object NO_RETURN_VALUE = new Integer (0);

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
		    final Object result = invokeMethod (target, method, arguments);
		    if (result != NO_RETURN_VALUE)
		    {
			return result;
		    }
		}
	    }
	}
	final Class<?> parentClass = cls.getSuperclass ();
	if (parentClass == null)
	{
	    throw new IllegalArgumentException ("Can't apply method to object");
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
	    if (actual == NO_RETURN_VALUE)
	    {
		return NO_RETURN_VALUE;
	    }
	    actuals[i] = actual;
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
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
	// [TODO] Handle char, long, short, boolean etc.
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
	return NO_RETURN_VALUE;
    }

    public Object describeEvaluator (final List<Object> arguments)
    {
	for (final Object arg : arguments)
	{
	    describe (arg);
	}
	return Boolean.FALSE;
    }

    private void describe (final Object arg)
    {
	System.out.printf ("Describe: %s \n", arg);
	if (arg == null)
	{

	}
	else if (arg instanceof Described)
	{
	    final Described d = (Described)arg;
	    d.describe ();
	}
	else
	{
	    System.out.printf ("Class: %s \n", arg.getClass ().getCanonicalName ());
	}
    }

    public Object getDefaultPackageEvaluator (final List<Object> arguments)
    {
	return PackageFactory.getDefaultPackage ();
    }

    public Object getSystemPackageEvaluator (final List<Object> arguments)
    {
	return PackageFactory.getSystemPackage ();
    }

    public Object getParentPackageEvaluator (final List<Object> arguments)
    {
	final Package p = getPackage (arguments, 0);
	return p.getParent ();
    }

    public Object getChildPackagesEvaluator (final List<Object> arguments)
    {
	final Package p = getPackage (arguments, 0);
	return p.getChildren ();
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
