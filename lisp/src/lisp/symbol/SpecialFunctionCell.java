/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.Method;
import java.util.*;

import org.objectweb.asm.tree.ClassNode;

import lisp.eval.LexicalContext;
import lisp.lang.Symbol;
import lisp.util.MultiMap;

public class SpecialFunctionCell extends FunctionCell
{
    public SpecialFunctionCell (final Symbol symbol)
    {
	super (symbol, false);
    }

    @Override
    public Overload overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
    {
	return super.overload (obj, method, documentation, source, cn);
    }

    @Override
    public Object eval (final LexicalContext context, final List<? extends Object> form) throws Exception
    {
	// Form size is one extra due to the function name &
	// Number of arguments is one extra due to the interpreter argument.
	final List<Object> actuals = new ArrayList<> ();
	actuals.add (context);
	for (int i = 1; i < form.size (); i++)
	{
	    actuals.add (form.get (i));
	}
	return apply (actuals);
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    @Override
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	super.getDescriberValues (result, target);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getFunctionName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
