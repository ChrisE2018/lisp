
package lisp.eval;

import java.lang.reflect.*;
import java.util.List;

import lisp.exceptions.CoerceError;
import lisp.lang.Symbol;
import lisp.symbol.Applicable;

public class Invoke
{
    private static Applicable applicable = new Applicable ();

    /**
     * Recursive method to perform a java method call. Actual arguments start at argument 0. This
     * attempts to select a method that matches the parameter types, but because of dynamic
     * conversion from objects there are ambiguous cases. In other words, without actual type
     * declarations for the arguments, it is not possible to completely match Java method
     * overloading semantics.
     *
     * @param target The object to invoke the method on.
     * @param cls The class of the target object.
     * @param methodName The name of the method to invoke.
     * @param arguments The fully evaluated arguments to apply.
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    public Object javaMethodCall (final Object target, final Class<?> cls, final String methodName, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	// Get local methods. Need to apply recursively to superclass
	final Method[] methods = cls.getDeclaredMethods ();
	for (final Method method : methods)
	{
	    if (method.getName ().equals (methodName))
	    {
		if (applicable.applicable (method, arguments))
		{
		    return apply (method, target, arguments);
		}
	    }
	}
	final Class<?> parentClass = cls.getSuperclass ();
	if (parentClass == null)
	{
	    throw new IllegalArgumentException ("Can't apply method '" + methodName + "' to object " + target);
	}
	return javaMethodCall (target, parentClass, methodName, arguments);
    }

    // /**
    // * Determine if an overloaded method is applicable to a specific set of arguments. Currently
    // * this just tests the number of arguments, it needs to be modified to test the types
    // too.</br>
    // * FIXME Test parameter types.
    // *
    // * @param method
    // * @param arguments
    // * @return
    // */
    // private boolean applicable (final Method method, final List<Object> arguments)
    // {
    // if (method.isVarArgs ())
    // {
    // if (method.getParameterCount () - 1 <= arguments.size ())
    // {
    // return true;
    // }
    // }
    // else if (method.getParameterCount () == arguments.size ())
    // {
    // return true;
    // }
    // return false;
    // }

    /**
     * Apply a single method to a target and argument list. This will only apply this method, it
     * will not search for an inherited method or another overloaded version.
     *
     * @param method The method to apply.
     * @param target The object to apply the method to.
     * @param arguments The arguments to pass. These will be coerced to the correct types if
     *            possible using narrowing conversions only. Widening conversions will not apply. By
     *            special arrangement, symbols can be coerced to Strings. Longs will be converted to
     *            float or double even if some precision is lost.
     * @return The result produced by the method.
     * @see coerceToParameter
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    public Object apply (final Method method, final Object target, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	// Need to handle VarArgs here.
	if (method.isVarArgs ())
	{
	    if (method.getParameterCount () - 1 <= arguments.size ())
	    {
		return invokeVarArgsMethod (target, method, arguments);
	    }
	}
	else if (method.getParameterCount () == arguments.size ())
	{
	    return invokeMethod (target, method, arguments);
	}
	throw new IllegalArgumentException ();
    }

    /**
     * Invoke a VarArgs method on computed arguments. This is the apply method specialized for the
     * VarArgs case.
     *
     * @param method The method to apply.
     * @param target The object to apply the method to.
     * @param arguments The arguments to pass. These will be coerced to the correct types if
     *            possible using narrowing conversions only. Widening conversions will not apply. By
     *            special arrangement, symbols can be coerced to Strings. Longs will be converted to
     *            float or double even if some precision is lost.
     * @return The result produced by the method.
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    private Object invokeVarArgsMethod (final Object target, final Method method, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] actuals = new Object[parameters.length];
	final int count = parameters.length;
	final int fixed = count - 1;
	for (int i = 0; i < fixed; i++)
	{
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
	    final Object arg = arguments.get (i);
	    final Object actual = coerceToParameter (parameters[i], arg);
	    actuals[i] = actual;
	}
	final Object[] tail = new Object[arguments.size () - fixed];
	// All remaining parameters must be of the tailClass
	final Class<?> tailClass = parameters[fixed].getComponentType ();
	for (int i = 0; i < tail.length; i++)
	{
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
	    final int ii = i + fixed;
	    final Object arg = arguments.get (ii);
	    final Object actual = coerceToParameter (tailClass, arg);
	    tail[i] = actual;
	}
	actuals[fixed] = tail;
	return method.invoke (target, actuals);
    }

    /**
     * Invoke a fixed parameter method on a target and arguments. This is the invoke method for the
     * fixed parameter case.
     *
     * @param method The method to apply.
     * @param target The object to apply the method to.
     * @param arguments The arguments to pass. These will be coerced to the correct types if
     *            possible using narrowing conversions only. Widening conversions will not apply. By
     *            special arrangement, symbols can be coerced to Strings. Longs will be converted to
     *            float or double even if some precision is lost.
     * @return The result produced by the method.
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    private Object invokeMethod (final Object target, final Method method, final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	final Class<?>[] parameters = method.getParameterTypes ();
	final Object[] actuals = new Object[parameters.length];
	for (int i = 0; i < parameters.length; i++)
	{
	    // Scan arguments and try to coerce to valid types.
	    // If all args can be coerced, then call the method.
	    final Object arg = arguments.get (i);
	    final Object actual = coerceToParameter (parameters[i], arg);
	    actuals[i] = actual;
	}
	final Object result = method.invoke (target, actuals);
	return result;
    }

    /**
     * Coerce a parameter to a required class. This is really a type checking operation, no actual
     * conversion takes place (except when converting symbols to String).
     *
     * @param toClass The class that is required for a method call.
     * @param arg This will be coerced to the correct type, if possible, using narrowing conversions
     *            only. Widening conversions will not be used. By special arrangement, symbols can
     *            be coerced to Strings. Longs will be converted to float or double even if some
     *            precision is lost.
     * @return The coerced parameter.
     * @throws CoerceError if the argument could not be converted.
     */
    private Object coerceToParameter (final Class<?> toClass, final Object arg)
    {
	if (toClass.equals (Object.class))
	{
	    return arg;
	}
	final Class<?> argClass = arg.getClass ();
	if (toClass == argClass)
	{
	    return arg;
	}
	if (toClass.isAssignableFrom (argClass))
	{
	    return arg;
	}
	if (toClass == String.class)
	{
	    // Handle String from Symbol
	    if (arg instanceof Symbol)
	    {
		return ((Symbol)arg).getName ();
	    }
	}
	if (toClass.isPrimitive ())
	{
	    // No simple solution:
	    // https://stackoverflow.com/questions/1704634/simple-way-to-get-wrapper-class-type-in-java
	    if (toClass == int.class)
	    {
		if (argClass == Integer.class || argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == boolean.class)
	    {
		if (argClass == Boolean.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == double.class)
	    {
		if (argClass == Double.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class
		    || argClass == Float.class || argClass == Long.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == long.class)
	    {
		if (argClass == Long.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == char.class)
	    {
		if (argClass == Character.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == byte.class)
	    {
		if (argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == short.class)
	    {
		if (argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (toClass == float.class)
	    {
		if (argClass == Float.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class
		    || argClass == Float.class || argClass == Long.class)
		{
		    return arg;
		}
	    }
	}
	throw new CoerceError ("Can't coerce %s to %s", arg, toClass);
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
