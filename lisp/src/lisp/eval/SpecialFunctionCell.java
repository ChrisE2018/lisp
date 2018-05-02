
package lisp.eval;

import java.lang.reflect.*;
import java.util.List;

/**
 * Function cell that processes the original form without evaluating arguments. Needed for control
 * constructs like if, and, or.
 */
public class SpecialFunctionCell extends FunctionCell
{
    private final Object obj;
    private final Method method;

    public SpecialFunctionCell (final Object obj, final Method method)
    {
	this.obj = obj;
	this.method = method;
    }

    @Override
    public Object eval (final Interpreter interpreter, final List<?> form)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Object result = method.invoke (obj, form);
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
