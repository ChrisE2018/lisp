
package lisp.eval;

import java.lang.reflect.Method;
import java.util.*;

import lisp.Symbol;

/**
 * Function cell that processes the original form and returns an expanded form for further
 * evaluation.
 */
public class MacroFunctionCell extends FunctionCell
{
    private final Object object;
    private final Method method;

    public MacroFunctionCell (final Symbol symbol, final Object obj, final Method method)
    {
	super (symbol);
	this.object = obj;
	this.method = method;
    }

    @Override
    public void overload (final DefineLisp a, final Object obj, final Method m)
    {
	throw new UnsupportedOperationException ("Can't overload macro functions");
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final Object expanded = method.invoke (object, form);
	final Object result = interpreter.eval (expanded);
	return result;
    }

    /**
     * Get a map describing an object. The return value is intended to be used by a debugger to
     * print an object decomposition.
     *
     * @param target
     * @return
     */
    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Object", object);
	result.put ("Method", method);
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getFunctionName ());
	buffer.append (" ");
	buffer.append (method);
	buffer.append (">");
	return buffer.toString ();
    }
}
