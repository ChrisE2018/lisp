
package lisp.eval;

import java.lang.reflect.Method;
import java.util.*;

/**
 * Function cell that processes the original form and returns an expanded form for further
 * evaluation.
 */
public class MacroFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;

    public MacroFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
    }

    @Override
    public void overload (final DefineLisp a, final Method m)
    {
	throw new UnsupportedOperationException ("Can't overload macro functions");
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form) throws Exception
    {
	final Object expanded = method.invoke (obj, form);
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
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Object", obj);
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
	buffer.append (method);
	buffer.append (">");
	return buffer.toString ();
    }
}
