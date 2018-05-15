
package lisp.eval;

import java.util.*;
import java.util.Map.Entry;

import lisp.*;
import lisp.Package;

/**
 * Bind java functions to lisp symbols.
 * <p>
 * Note that special forms like quote and setq are passed the interpreter as a first parameter and
 * the entire expression (including the function name) as the arguments. The real arguments start at
 * the second position. This is convenient because the interpreter is already processing the whole
 * expression and would have to allocate a new object to remove the function from the start of the
 * expression.
 * </p>
 * <p>
 * Normal functions are just passed the evaluated arguments. The real arguments start at the
 * beginning. This is convenient since the interpreter creates a new list for the arguments anyhow.
 * </p>
 */
public class Primitives extends Definer
{
    // [TODO] Create immutable list
    // Property list access
    // [TODO] Maps, sets, union, intersection, difference
    // [TODO] Package creation, uses, export, import
    // [TODO] File functions

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

    /**
     * Interpreter for function definitions.
     *
     * @param interpreter Not used, but required by calling protocol.
     */
    @DefineLisp (special = true, name = "def")
    public Object defEvaluator (final Interpreter interpreter, final Symbol name, final LispList arglist, final Object... body)
    {
	final List<Symbol> params = new ArrayList<Symbol> ();
	for (final Object a : arglist)
	{
	    params.add ((Symbol)a);
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
    @DefineLisp (special = true)
    public Object setq (final Interpreter interpreter, final Symbol symbol, final Object form) throws Exception
    {
	final Object value = interpreter.eval (form);
	symbol.setValue (value);
	return value;
    }

    @DefineLisp
    public Object list (final Object... arguments)
    {
	return new LispList (arguments);
    }

    @DefineLisp
    public Object not (final Object arg)
    {
	if (arg instanceof Boolean)
	{
	    final Boolean b = (Boolean)arg;
	    return !b;
	}
	return Boolean.FALSE;
    }

    @DefineLisp (name = "null")
    public Object nullPredicate (final Object arg)
    {
	return arg == null;
    }

    @DefineLisp (special = true)
    public Object or (final Interpreter interpreter, final Object... arguments) throws Exception
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = interpreter.eval (arg);
	    if (isTrue (value))
	    {
		return value;
	    }
	}
	return false;
    }

    @DefineLisp (special = true)
    public Object and (final Interpreter interpreter, final Object... arguments) throws Exception
    {
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = interpreter.eval (arg);
	    if (!isTrue (value))
	    {
		return false;
	    }
	    result = value;
	}
	return result;
    }

    @DefineLisp (special = true, name = "if")
    public Object ifEvaluator (final Interpreter interpreter, final Object test, final Object trueClause,
            final Object... arguments) throws Exception
    {
	if (isTrue (interpreter.eval (test)))
	{
	    return interpreter.eval (trueClause);
	}
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
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

    @DefineLisp (special = true, name = "when")
    public Object whenForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	if (isTrue (interpreter.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	    return result;
	}
	return false;
    }

    @DefineLisp (special = true, name = "unless")
    public Object unlessForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	if (!isTrue (interpreter.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	    return result;
	}
	return false;
    }

    @DefineLisp (special = true, name = "while")
    public Object whileForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	Object result = true;
	while (isTrue (interpreter.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	}
	return result;
    }

    @DefineLisp (special = true, name = "until")
    public Object untilForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	Object result = true;
	while (!isTrue (interpreter.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	}
	return result;
    }

    @DefineLisp
    public Object symbolValue (final Symbol arg)
    {
	return arg.getValue ();
    }

    @DefineLisp
    public Object symbolFunction (final Symbol arg)
    {
	return arg.getFunction ();
    }

    @DefineLisp
    public Object symbolPlist (final Symbol arg)
    {
	return arg.getPlist ();
    }

    @DefineLisp
    public Object get (final Symbol arg, final Symbol key)
    {
	return arg.get (key);
    }

    @DefineLisp
    public Object put (final Symbol arg, final Symbol key, final Object value)
    {
	return arg.put (key, value);
    }

    @DefineLisp
    public Object remove (final Symbol arg, final Symbol key)
    {
	return arg.remove (key);
    }

    @DefineLisp
    public Object inPackage (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	PackageFactory.setDefaultPackage (p);
	return pkg;
    }

    /**
     * Lookup and return the default (user) package.
     *
     * @param arguments
     */
    @DefineLisp
    public Object getDefaultPackage ()
    {
	return PackageFactory.getDefaultPackage ();
    }

    /**
     * Lookup and return the system package.
     *
     * @param arguments
     */
    @DefineLisp
    public Object getSystemPackage ()
    {
	return PackageFactory.getSystemPackage ();
    }

    @DefineLisp
    public Object getParentPackages (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	final LispList result = new LispList ();
	result.addAll (p.getParents ());
	return result;
    }

    @DefineLisp
    public Object getChildPackages (final Object pkg)
    {
	final Package p = coercePackage (pkg, true);
	final LispList result = new LispList ();
	result.addAll (p.getChildren ());
	return result;
    }

    @DefineLisp
    public Object printf (final String format, final Object... arguments)
    {
	System.out.printf (format, arguments);
	return null;
    }

    @DefineLisp
    public Object describe (final Object arg)
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
	return Boolean.FALSE;
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
