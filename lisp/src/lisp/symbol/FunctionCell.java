/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.*;
import java.util.function.Predicate;

import org.objectweb.asm.tree.ClassNode;

import lisp.*;
import lisp.cc.LocalBinding;
import lisp.eval.LexicalContext;
import lisp.util.MultiMap;

/** Base class of all function cells. */
public abstract class FunctionCell implements Describer
{
    private static Applicable applicable = new Applicable ();
    private static Selectable selectable = new Selectable ();

    /** The symbol this function cell is attached to. */
    private final Symbol symbol;

    private final boolean allowRedefinition;

    private final List<ObjectMethod> overloads = new ArrayList<ObjectMethod> ();

    /** Optional support object with compiler and analyzer. */
    private LispFunction lispFunction;

    abstract public Object eval (final LexicalContext context, final List<? extends Object> list) throws Exception;

    /**
     * Add an overloaded definition to this function cell. The documentation and ASM class node will
     * be attached for reference.
     *
     * @param obj The object that is used to invoke the method.
     * @param method The method to use when calling this FunctionCell.
     * @param documentation Documentation string for display.
     * @param source The Lisp source object.
     * @param cn An ASM ClassNode containing the bytecode.
     */
    public void overload (final Object obj, final Method method, final String documentation, final Object source,
            final ClassNode cn)
    {
	final ObjectMethod newOverload = new ObjectMethod (obj, method, documentation, source, cn);
	final String signature = newOverload.getArgumentSignature ();
	overloads.removeIf (new Predicate<ObjectMethod> ()
	{
	    @Override
	    public boolean test (final ObjectMethod t)
	    {
		final boolean result = t.getArgumentSignature ().equals (signature);
		if (result)
		{
		    // Invalidate the overload that is about to be removed.
		    t.valid = false;
		}
		return result;
	    }
	});
	// Want to remove this block of code and rely on the above:
	// final int count = newOverload.getParameterTypes ().length;
	// overloads.removeIf (new Predicate<ObjectMethod> ()
	// {
	// @Override
	// public boolean test (final ObjectMethod t)
	// {
	// return t.getParameterTypes ().length == count;
	// }
	// });
	overloads.add (newOverload);
    }

    FunctionCell (final Symbol symbol, final boolean allowRedefinition)
    {
	this.symbol = symbol;
	this.allowRedefinition = allowRedefinition;
    }

    public Symbol getFunctionName ()
    {
	return symbol;
    }

    public boolean isAllowRedefinition ()
    {
	return allowRedefinition;
    }

    /** Optional support object with compiler and analyzer. */
    public LispFunction getLispFunction ()
    {
	return lispFunction;
    }

    /** Optional support object with compiler and analyzer. */
    public void setLispFunction (final LispFunction lispFunction)
    {
	if (this.lispFunction != null)
	{
	    throw new Error ("Lisp function support has already been defined for " + this);
	}
	this.lispFunction = lispFunction;
    }

    /**
     * Determine the overload used when this function is called on an expression. FIXME Special
     * functions need to implement their own version of this.
     *
     * @param locals The current binding context.
     * @param expression The expression that will be evaluated.
     */
    public ObjectMethod selectMethod (final Map<Symbol, LocalBinding> locals, final LispList expression)
    {
	final List<Class<?>> arguments = new ArrayList<Class<?>> ();
	for (int i = 1; i < expression.size (); i++)
	{
	    final Object arg = expression.get (i);
	    final Class<?> argClass = predictResultClass (locals, arg);
	    arguments.add (argClass);
	}
	ObjectMethod selectedMethod = null;
	for (final ObjectMethod method : overloads)
	{
	    final Method m = method.getMethod ();
	    if (selectable.isSelectable (m, arguments))
	    {
		if (selectedMethod == null)
		{
		    selectedMethod = method;
		}
		else if (selectedMethod.isBetterThan (method))
		{
		    // Ignore
		}
		else if (method.isBetterThan (selectedMethod))
		{
		    selectedMethod = method;
		}
		else
		{
		    final StringBuilder buffer = new StringBuilder ();
		    buffer.append ("Ambiguous ");
		    buffer.append (symbol);
		    buffer.append (" method selection for ");
		    buffer.append (arguments);
		    buffer.append (". Both ");
		    buffer.append (selectedMethod.getArgumentSignature ());
		    buffer.append (" and ");
		    buffer.append (method.getArgumentSignature ());
		    buffer.append (" apply.");
		    throw new IllegalArgumentException (buffer.toString ());
		}
	    }
	}
	return selectedMethod;
    }

    /**
     * Determine the return class of an expression. @param locals The current binding context.
     *
     * @param expression The expression that will be evaluated.
     */
    private Class<?> predictResultClass (final Map<Symbol, LocalBinding> locals, final Object expression)
    {
	if (expression instanceof List)
	{
	    final LispList expr = (LispList)expression;
	    final Symbol fn = expr.head ();
	    final FunctionCell function = fn.getFunction ();
	    if (function != null)
	    {
		final Class<?> result = function.getResultClass (locals, expr);
		if (result != null)
		{
		    return result;
		}
	    }
	    // Function call always returns a value whether we want it or not
	    return Object.class;
	}
	if (expression instanceof Symbol)
	{
	    // Variable reference
	    final Symbol var = (Symbol)expression;
	    if (locals.containsKey (var))
	    {
		// Reference to a local lexical variable
		final LocalBinding lb = locals.get (var);
		return lb.getVariableClass ();
	    }
	    else if (var.is ("true") || var.is ("t") || var.is ("false") || var.is ("f"))
	    {
		return Boolean.class;
	    }
	    else
	    {
		// Reference to a global variable
		final Object value = var.getValue (null);
		if (value instanceof TypedValueCell)
		{
		    final TypedValueCell tvc = (TypedValueCell)value;
		    return tvc.getValueType ();
		}
		return Object.class;
	    }
	}
	else if (expression == null)
	{
	    return void.class;
	}
	else
	{
	    return expression.getClass ();
	}
    }

    /**
     * Determine the result class when this function is called on an expression.
     *
     * @param locals The current binding context.
     * @param expression The expression that will be evaluated.
     */
    private Class<?> getResultClass (final Map<Symbol, LocalBinding> locals, final LispList expression)
    {
	if (overloads.size () > 0)
	{
	    final ObjectMethod result = selectMethod (locals, expression);
	    if (result != null)
	    {
		return result.getMethod ().getReturnType ();
	    }
	}
	return Object.class;
    }

    /**
     * This is called from compiled code and translates to the list form now implemented. This
     * should probably be Deprecated and eliminated.
     */
    public Object apply (final Object[] arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final List<Object> actuals = new ArrayList<Object> ();
	for (final Object arg : arguments)
	{
	    actuals.add (arg);
	}
	return apply (actuals);
    }

    /**
     * Apply this FunctionCell to a specific set of arguments. If several overloads can apply, the
     * first one wins. This might not be the expected result. For example, if there are two one
     * parameter overloads and one takes a double value and the second takes an int, then the result
     * is dependent on the order they are defined. The double value version can steal calls to the
     * int value version.
     *
     * @param arguments Arguments to pass to the method. These have already been evaluated. An
     *            applicable overload is selected or IllegalArgumentException is thrown if no
     *            overload can be used.
     * @return The result of the method call.
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    public Object apply (final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	ObjectMethod selectedMethod = null;
	for (final ObjectMethod method : overloads)
	{
	    final Method m = method.getMethod ();
	    if (applicable.applicable (m, arguments))
	    {
		if (selectedMethod == null)
		{
		    selectedMethod = method;
		}
		else if (selectedMethod.isBetterThan (method))
		{
		    // Ignore. Test this first so method definition order is secondary key.
		}
		else if (method.isBetterThan (selectedMethod))
		{
		    // (define foo (int:a double:b) 'alpha)
		    // (define foo (double:a int:b) 'beta)
		    selectedMethod = method;
		}
		else
		{
		    final StringBuilder buffer = new StringBuilder ();
		    buffer.append ("Ambiguous ");
		    buffer.append (symbol);
		    buffer.append (" method selection for ");
		    buffer.append (arguments);
		    buffer.append (". Both ");
		    buffer.append (selectedMethod.getArgumentSignature ());
		    buffer.append (" and ");
		    buffer.append (method.getArgumentSignature ());
		    buffer.append (" apply.");
		    throw new IllegalArgumentException (buffer.toString ());
		}
	    }
	}
	if (selectedMethod != null)
	{
	    return selectedMethod.apply (arguments);
	}
	throw new IllegalArgumentException ("No applicable " + symbol + " method for " + arguments);
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
    {
	result.put ("Symbol", symbol);
	result.put ("AllowRedefinition", allowRedefinition);
	if (lispFunction != null)
	{
	    result.put ("Function", lispFunction);
	}
	for (final ObjectMethod method : overloads)
	{
	    result.put ("Overload", method);
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (symbol);
	buffer.append (">");
	return buffer.toString ();
    }
}
