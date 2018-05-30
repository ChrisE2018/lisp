
package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;

import lisp.Symbol;
import lisp.eval.LexicalContext;

/**
 * Function cell that processes the original form and returns an expanded form for further
 * evaluation.
 */
public class MacroFunctionCell extends FunctionCell
{
    private final Object object;
    private final Method method;
    private final String documentation;

    public MacroFunctionCell (final Symbol symbol, final Object obj, final Method method, final String documentation)
    {
	super (symbol, false);
	object = obj;
	this.method = method;
	this.documentation = documentation;
    }

    @Override
    public void overload (final Object obj, final Method m, final String doc)
    {
	throw new UnsupportedOperationException ("Can't overload macro functions");
    }

    @Override
    public Object eval (final LexicalContext context, final List<?> form) throws Exception
    {
	final Object expanded = method.invoke (object, form);
	final Object result = context.eval (expanded);
	return result;
    }

    public Object expand (final List<?> form) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	return method.invoke (object, form);
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    @Override
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	super.getDescriberValues (result, target);
	result.put ("Object", object);
	result.put ("Method", method);
	result.put ("Documentation", documentation);
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
