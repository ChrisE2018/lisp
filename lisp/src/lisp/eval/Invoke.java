
package lisp.eval;

import java.lang.reflect.*;
import java.util.List;

import lisp.Symbol;

public class Invoke
{
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
		if (applicable (method, target, arguments))
		{
		    return apply (method, target, arguments);
		}
		// Need to handle VarArgs here.
		// if (method.isVarArgs ())
		// {
		// if (method.getParameterCount () >= arguments.size ())
		// {
		// try
		// {
		// return invokeVarArgsMethod (target, method, arguments);
		// }
		// catch (final CoerceError e)
		// {
		//
		// }
		// }
		// }
		// else if (method.getParameterCount () == arguments.size ())
		// {
		// try
		// {
		// return invokeMethod (target, method, arguments);
		// }
		// catch (final CoerceError e)
		// {
		//
		// }
		// }
	    }
	}
	final Class<?> parentClass = cls.getSuperclass ();
	if (parentClass == null)
	{
	    throw new IllegalArgumentException ("Can't apply method '" + methodName + "' to object " + target);
	}
	return javaMethodCall (target, parentClass, methodName, arguments);
    }

    public boolean applicable (final Method method, final Object target, final List<Object> arguments)
    {
	if (method.isVarArgs ())
	{
	    if (method.getParameterCount () - 1 <= arguments.size ())
	    {
		return true;
	    }
	}
	else if (method.getParameterCount () == arguments.size ())
	{
	    return true;
	}
	return false;
    }

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
     * Invoke a VarArgs method on computed arguments.
     *
     * @param target
     * @param method
     * @param arguments
     * @return
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    public Object invokeVarArgsMethod (final Object target, final Method method, final List<Object> arguments)
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

    public Object invokeMethod (final Object target, final Method method, final List<Object> arguments)
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

    public Object coerceToParameter (final Class<?> p, final Object arg)
    {
	if (p.equals (Object.class))
	{
	    return arg;
	}
	final Class<?> argClass = arg.getClass ();
	if (p == argClass)
	{
	    return arg;
	}
	if (p.isAssignableFrom (argClass))
	{
	    return arg;
	}
	if (p == String.class)
	{
	    // Handle String from Symbol
	    if (arg instanceof Symbol)
	    {
		return ((Symbol)arg).getName ();
	    }
	}
	if (p.isPrimitive ())
	{
	    // No simple solution:
	    // https://stackoverflow.com/questions/1704634/simple-way-to-get-wrapper-class-type-in-java
	    if (p == int.class)
	    {
		if (argClass == Integer.class || argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (p == boolean.class)
	    {
		if (argClass == Boolean.class)
		{
		    return arg;
		}
	    }
	    else if (p == double.class)
	    {
		if (argClass == Double.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class
		    || argClass == Float.class || argClass == Long.class)
		{
		    return arg;
		}
	    }
	    else if (p == long.class)
	    {
		if (argClass == Long.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (p == char.class)
	    {
		if (argClass == Character.class)
		{
		    return arg;
		}
	    }
	    else if (p == byte.class)
	    {
		if (argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (p == short.class)
	    {
		if (argClass == Short.class || argClass == Byte.class)
		{
		    return arg;
		}
	    }
	    else if (p == float.class)
	    {
		if (argClass == Float.class || argClass == Integer.class || argClass == Short.class || argClass == Byte.class
		    || argClass == Float.class || argClass == Long.class)
		{
		    return arg;
		}
	    }
	}
	throw new CoerceError ("Can't coerce %s to %s", arg, p);
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
