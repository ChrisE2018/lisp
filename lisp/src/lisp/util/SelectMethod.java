
package lisp.util;

import java.lang.reflect.*;
import java.util.*;

import lisp.*;
import lisp.cc.LexicalBinding;
import lisp.symbol.*;

public class SelectMethod
{
    private static MethodSignature methSignature = new MethodSignature ();
    private static Selectable selectable = new Selectable ();
    private static Assignable assignable = new Assignable ();

    /**
     * Determine the overload used when this function is called on an expression. FIXME Special
     * functions need to implement their own version of this.
     *
     * @param locals The current binding context.
     * @param expression The expression that will be evaluated.
     */
    public Method selectStaticMethod (final Class<?> claz, final String methodName, final Map<Symbol, LexicalBinding> locals,
            final LispList expression)
    {
	final List<Class<?>> arguments = new ArrayList<Class<?>> ();
	for (int i = 1; i < expression.size (); i++)
	{
	    final Object arg = expression.get (i);
	    final Class<?> argClass = predictResultClass (locals, arg);
	    arguments.add (argClass);
	}
	Method selectedMethod = null;
	for (final Method method : claz.getDeclaredMethods ())
	{
	    if (Modifier.isStatic (method.getModifiers ()))
	    {
		if (selectable.isSelectable (method, arguments))
		{
		    if (selectedMethod == null)
		    {
			selectedMethod = method;
		    }
		    else if (isBetterThan (selectedMethod, method))
		    {
			// Ignore
		    }
		    else if (isBetterThan (method, selectedMethod))
		    {
			selectedMethod = method;
		    }
		    else
		    {
			final StringBuilder buffer = new StringBuilder ();
			buffer.append ("Ambiguous ");
			buffer.append (methodName);
			buffer.append (" method selection for ");
			buffer.append (arguments);
			buffer.append (". Both ");
			buffer.append (methSignature.getArgumentSignature (selectedMethod));
			buffer.append (" and ");
			buffer.append (methSignature.getArgumentSignature (method));
			buffer.append (" apply.");
			throw new IllegalArgumentException (buffer.toString ());
		    }
		}
	    }
	}
	// No search up to superclass
	return selectedMethod;
    }

    /**
     * Determine the overload used when this function is called on an expression. FIXME Special
     * functions need to implement their own version of this.
     *
     * @param locals The current binding context.
     * @param expression The expression that will be evaluated.
     */
    public Method selectMethod (final Class<?> claz, final String methodName, final Map<Symbol, LexicalBinding> locals,
            final LispList expression)
    {
	final List<Class<?>> arguments = new ArrayList<Class<?>> ();
	for (int i = 1; i < expression.size (); i++)
	{
	    final Object arg = expression.get (i);
	    final Class<?> argClass = predictResultClass (locals, arg);
	    arguments.add (argClass);
	}
	Method selectedMethod = null;
	for (final Method method : claz.getDeclaredMethods ())
	{
	    if (selectable.isSelectable (method, arguments))
	    {
		if (selectedMethod == null)
		{
		    selectedMethod = method;
		}
		else if (isBetterThan (selectedMethod, method))
		{
		    // Ignore
		}
		else if (isBetterThan (method, selectedMethod))
		{
		    selectedMethod = method;
		}
		else
		{
		    final StringBuilder buffer = new StringBuilder ();
		    buffer.append ("Ambiguous ");
		    buffer.append (methodName);
		    buffer.append (" method selection for ");
		    buffer.append (arguments);
		    buffer.append (". Both ");
		    buffer.append (methSignature.getArgumentSignature (selectedMethod));
		    buffer.append (" and ");
		    buffer.append (methSignature.getArgumentSignature (method));
		    buffer.append (" apply.");
		    throw new IllegalArgumentException (buffer.toString ());
		}
	    }
	}
	if (selectedMethod != null)
	{
	    return selectedMethod;
	}
	final Class<?> superclass = claz.getSuperclass ();
	if (superclass != null)
	{
	    return selectMethod (superclass, methodName, locals, expression);
	}
	return null;
    }

    /**
     * Determine the return class of an expression. @param locals The current binding context.
     *
     * @param expression The expression that will be evaluated.
     */
    public Class<?> predictResultClass (final Map<Symbol, LexicalBinding> locals, final Object expression)
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
		final LexicalBinding lb = locals.get (var);
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
     * Determine if this method overload is better than another viable candidate.
     *
     * @param otherMethod Another overload option that is known to be applicable to an argument set.
     * @return True if this method overload should be used in preference to the other overload.
     */
    public boolean isBetterThan (final Method method, final Method otherMethod)
    {
	final Class<?>[] myTypes = method.getParameterTypes ();
	final Class<?>[] otherTypes = otherMethod.getParameterTypes ();
	if (myTypes.length < otherTypes.length)
	{
	    // Prefer fixed argument methods to VarArgs methods.
	    return true;
	}
	else if (otherTypes.length < myTypes.length)
	{
	    // Prefer fixed argument methods to VarArgs methods.
	    return false;
	}
	for (int i = 0; i < myTypes.length; i++)
	{
	    final Class<?> myType = myTypes[i];
	    final Class<?> otherType = otherTypes[i];
	    if (!assignable.isAssignableFrom (myType, otherType))
	    {
		return true;
	    }
	}
	return false;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
