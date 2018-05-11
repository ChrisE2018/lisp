
package lisp.eval;

import java.util.*;
import java.util.Map.Entry;

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

    /**
     * Bind java functions to lisp symbols.
     * <p>
     * Note that special forms like quote and setq are passed the interpreter as a first parameter
     * and the entire expression (including the function name) as the arguments. The real arguments
     * start at the second position. This is convenient because the interpreter is already
     * processing the whole expression and would have to allocate a new object to remove the
     * function from the start of the expression.
     * </p>
     * <p>
     * Normal functions are just passed the evaluated arguments. The real arguments start at the
     * beginning. This is convenient since the interpreter creates a new list for the arguments
     * anyhow.
     * </p>
     *
     * @throws NoSuchMethodException
     * @throws SecurityException
     */
    private void definePrimitives () throws NoSuchMethodException, SecurityException
    {
	defspecial ("quote", "quoteEvaluator");
	defspecial ("def", "defEvaluator");
	defspecial ("setq", "setqEvaluator");
	define ("list", "listEvaluator");
	// [TODO] Create immutable list
	define ("plus", "plusEvaluator");
	define ("+", "plusEvaluator");
	define ("times", "timesEvaluator");
	define ("*", "timesEvaluator");
	// [TODO] subtraction, division, comparison, trig, abs
	defineTyped ("not", "notEvaluatorT");
	defspecial ("or", "orEvaluator");
	defspecial ("and", "andEvaluator");
	defspecial ("if", "ifEvaluator");
	// [TODO] Property list access
	// [TODO] Maps, sets, union, intersection, difference
	defineTyped ("in-package", "inPackageEvaluatorT");
	// [TODO] Package creation, uses, export, import
	defineTyped ("symbolValue", "symbolValueEvaluatorT");
	defineTyped ("symbolFunction", "symbolFunctionEvaluatorT");
	defineTyped ("symbolPlist", "symbolFunctionEvaluatorT");
	defineTyped ("get", "symbolGetEvaluatorT");
	defineTyped ("put", "symbolPutEvaluatorT");
	defineTyped ("remove", "symbolRemoveEvaluatorT");
	defineTyped ("describe", "describeEvaluatorT");
	defineTyped ("getDefaultPackage", "getDefaultPackageEvaluatorT");
	defineTyped ("getSystemPackage", "getSystemPackageEvaluatorT");
	defineTyped ("getParentPackages", "getParentPackagesEvaluatorT");
	defineTyped ("getChildPackages", "getChildPackagesEvaluatorT");
	// [TODO] File functions
	define ("printf", "printfEvaluator");
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

    // public Object notEvaluator (final List<Object> arguments)
    // {
    // if (arguments.size () != 1)
    // {
    // throw new IllegalArgumentException ("Exactly one argument required");
    // }
    // final Object arg = arguments.get (0);
    // if (arg instanceof Boolean)
    // {
    // final Boolean b = (Boolean)arg;
    // return !b;
    // }
    // return Boolean.FALSE;
    // }

    public Object notEvaluatorT (final Object arg)
    {
	if (arg instanceof Boolean)
	{
	    final Boolean b = (Boolean)arg;
	    return !b;
	}
	return Boolean.FALSE;
    }

    public Object orEvaluator (final Interpreter interpreter, final List<Object> arguments) throws Exception
    {
	for (int i = 1; i < arguments.size (); i++)
	{
	    final Object arg = arguments.get (i);
	    final Object value = interpreter.eval (arg);
	    if (isTrue (value))
	    {
		return value;
	    }
	}
	return false;
    }

    public Object andEvaluator (final Interpreter interpreter, final List<Object> arguments) throws Exception
    {
	Object result = Boolean.TRUE;
	for (int i = 1; i < arguments.size (); i++)
	{
	    final Object arg = arguments.get (i);
	    final Object value = interpreter.eval (arg);
	    if (!isTrue (value))
	    {
		return false;
	    }
	    result = value;
	}
	return result;
    }

    public Object ifEvaluator (final Interpreter interpreter, final List<Object> arguments) throws Exception
    {
	final Object test = interpreter.eval (arguments.get (1));
	if (isTrue (test))
	{
	    return interpreter.eval (arguments.get (2));
	}
	Object result = Boolean.TRUE;
	for (int i = 3; i < arguments.size (); i++)
	{
	    final Object arg = arguments.get (i);
	    final Object value = interpreter.eval (arg);
	    result = value;
	}
	return result;
    }

    private boolean isTrue (final Object value)
    {
	if (value != null)
	{
	    if (value instanceof Boolean)
	    {
		if (false == (Boolean)value)
		{
		    return false;
		}
	    }
	    return true;
	}
	return false;
    }

    public Object inPackageEvaluatorT (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	PackageFactory.setDefaultPackage (p);
	return pkg;
    }

    public Object symbolValueEvaluatorT (final Symbol arg)
    {
	return arg.getValue ();
    }

    public Object symbolFunctionEvaluatorT (final Symbol arg)
    {
	return arg.getFunction ();
    }

    public Object symbolPlistEvaluatorT (final Symbol arg)
    {
	return arg.getPlist ();
    }

    public Object symbolGetEvaluatorT (final Symbol arg, final Symbol key)
    {
	return arg.get (key);
    }

    public Object symbolPutEvaluatorT (final Symbol arg, final Symbol key, final Object value)
    {
	return arg.put (key, value);
    }

    public Object symbolRemoveEvaluatorT (final Symbol arg, final Symbol key)
    {
	return arg.remove (key);
    }

    public Object describeEvaluatorT (final Object arg)
    {
	describe (arg);
	return Boolean.FALSE;
    }

    private void describe (final Object arg)
    {
	System.out.printf ("Describe: %s \n", arg);
	if (arg != null)
	{
	    final Describer d = getDescriber (arg);
	    if (d != null)
	    {
		describe (d, arg);
	    }
	    else
	    {
		System.out.printf ("Class: %s \n", arg.getClass ().getCanonicalName ());
	    }
	}
    }

    private Describer getDescriber (final Object arg)
    {
	if (arg instanceof Describer)
	{
	    return (Describer)arg;
	}
	if (arg instanceof List)
	{
	    return new LispList ();
	}
	// [TODO] Java bean describer
	return null;
    }

    private void describe (final Describer d, final Object arg)
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	int index = 0;
	final Map<String, Object> description = d.getDescriberValues (arg);
	for (final Entry<String, Object> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    ++index;
	    final String key = entry.getKey ();
	    final Object value = entry.getValue ();
	    final String doc = d.getDescriberDocumentation (arg, key);
	    final Symbol symbol = pkg.internPrivate (String.format ("d%d", index));
	    symbol.setValue (value);
	    if (doc != null)
	    {
		System.out.printf ("[%5s] %s: %s %s\n", symbol, key, value, doc);
	    }
	    else
	    {
		final String type = value == null ? "null" : value.getClass ().getSimpleName ();
		System.out.printf ("[%5s] %s: %s (%s)\n", symbol, key, value, type);
	    }
	}
    }

    /**
     * Lookup and return the default (user) package.
     *
     * @param arguments
     */
    public Object getDefaultPackageEvaluatorT ()
    {
	return PackageFactory.getDefaultPackage ();
    }

    /**
     * Lookup and return the system package.
     *
     * @param arguments
     */
    public Object getSystemPackageEvaluatorT ()
    {
	return PackageFactory.getSystemPackage ();
    }

    public Object getParentPackagesEvaluatorT (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	final LispList result = new LispList ();
	result.addAll (p.getParents ());
	return result;
    }

    public Object getChildPackagesEvaluatorT (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	final LispList result = new LispList ();
	result.addAll (p.getChildren ());
	return result;
    }

    public Object printfEvaluator (final List<Object> arguments)
    {
	final String format = getString (arguments, 0);
	final int size = arguments.size ();
	final Object[] args = new Object[size - 1];
	arguments.subList (1, size).toArray (args);
	System.out.printf (format, args);
	return null;
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
