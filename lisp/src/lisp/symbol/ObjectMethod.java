
package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import lisp.*;
import lisp.eval.Invoke;
import lisp.util.*;

/**
 * Combination of Object and Method that can be called as an overloaded function. This should be
 * renamed to Overload.
 *
 * @author cre
 */
public class ObjectMethod implements Describer
{
    private static Invoke invoke = new Invoke ();
    private static Boxing boxing = new Boxing ();

    final Object object;
    final Method method;
    final String documentation;

    /** Place to store the lisp source, if available. */
    final Object source;

    /** Place to store the ASM bytecode, if available. */
    final ClassNode cn;

    /** The minimum number of arguments that can be passed to this method. */
    private final int minimumArgCount;

    /**
     * If this method overload is replaced, set this to false. Compiled code can be setup to check
     * this flag and repair a compiled call to this method if it changes.
     */
    boolean valid = true;

    ObjectMethod (final Object object, final Method method, final String documentation, final Object source, final ClassNode cn)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
	this.source = source;
	this.cn = cn;
	minimumArgCount = method.getParameterCount () + (method.isVarArgs () ? -1 : 0);
    }

    ObjectMethod (final Object object, final Method method, final String documentation)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
	source = null;
	cn = null;
	minimumArgCount = method.getParameterCount () + (method.isVarArgs () ? -1 : 0);
    }

    public Object getObject ()
    {
	return object;
    }

    public Method getMethod ()
    {
	return method;
    }

    public String getDocumentation ()
    {
	return documentation;
    }

    public String getMethodName ()
    {
	return method.getName ();
    }

    public boolean isVarArgs ()
    {
	return method.isVarArgs ();
    }

    public Class<?>[] getParameterTypes ()
    {
	return method.getParameterTypes ();
    }

    /**
     * Determine if this method overload is applicable to the provided arguments.
     *
     * @param arguments The arguments that will be used to invoke the method.
     * @return True if it is applicable.
     */
    public boolean applicable (final List<Object> arguments)
    {
	final int count = arguments.size ();
	if (count < minimumArgCount)
	{
	    return false;
	}
	if (count > minimumArgCount)
	{
	    if (!method.isVarArgs ())
	    {
		return false;
	    }
	}
	// Additional filters here.
	final Class<?>[] types = method.getParameterTypes ();
	for (int i = 0; i < minimumArgCount; i++)
	{
	    final Class<?> argType = types[i]; // What is needed
	    final Object arg = arguments.get (i);
	    if (!isAssignableFrom (argType, arg))
	    {
		return false;
	    }
	}
	if (count > minimumArgCount && method.isVarArgs ())
	{
	    final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	    for (int i = minimumArgCount; i < arguments.size (); i++)
	    {
		final Object arg = arguments.get (i);
		if (!isAssignableFrom (argType, arg))
		{
		    return false;
		}
	    }
	}
	return true;
    }

    public boolean isSelectable (final List<Class<?>> arguments)
    {
	final int count = arguments.size ();
	if (count < minimumArgCount)
	{
	    return false;
	}
	if (count > minimumArgCount)
	{
	    if (!method.isVarArgs ())
	    {
		return false;
	    }
	}
	// Additional filters here.
	final Class<?>[] types = method.getParameterTypes ();
	for (int i = 0; i < minimumArgCount; i++)
	{
	    final Class<?> argType = types[i]; // What is needed
	    final Class<?> argClass = arguments.get (i);
	    if (!isAssignableFrom (argType, argClass))
	    {
		return false;
	    }
	}
	if (count > minimumArgCount && method.isVarArgs ())
	{
	    final Class<?> argType = types[minimumArgCount].getComponentType (); // What is needed
	    for (int i = minimumArgCount; i < arguments.size (); i++)
	    {
		final Class<?> argClass = arguments.get (i);
		if (!isAssignableFrom (argType, argClass))
		{
		    return false;
		}
	    }
	}
	return true;
    }

    /**
     * Determine if this method overload is better than another viable candidate.
     *
     * @param otherMethod Another overload option that is known to be applicable to an argument set.
     * @return True if this method overload should be used in preference to the other overload.
     */
    public boolean isBetterThan (final ObjectMethod otherMethod)
    {
	final Class<?>[] myTypes = method.getParameterTypes ();
	final Class<?>[] otherTypes = otherMethod.getParameterTypes ();
	for (int i = 0; i < myTypes.length; i++)
	{
	    final Class<?> myType = myTypes[i];
	    final Class<?> otherType = otherTypes[i];
	    if (!isAssignableFrom (myType, otherType))
	    {
		return true;
	    }
	}
	return false;
    }

    /**
     * Determine if a formal parameter can be assigned from an actual parameter. Null values cannot
     * be assigned to primitive types. Primitive types will support widening conversions, but not
     * allow narrowing. The Symbol class is treated specially and can be assigned to a String
     * parameter.
     *
     * @param toClass The class of the formal parameter.
     * @param from The actual parameter value.
     * @return True if the assignment is possible.
     */
    private boolean isAssignableFrom (final Class<?> toClass, final Object from)
    {
	if (from == null)
	{
	    return !toClass.isPrimitive ();
	}
	final Class<?> fromClass = from.getClass ();
	return isAssignableFrom (toClass, fromClass);
    }

    private boolean isAssignableFrom (final Class<?> toClass, final Class<?> fromClass)
    {
	if (toClass.isAssignableFrom (fromClass))
	{
	    return true;
	}
	if (toClass == String.class && fromClass == Symbol.class)
	{
	    return true;
	}
	if (toClass.isPrimitive ())
	{
	    if (fromClass.isPrimitive ())
	    {
		return isPrimitiveAssignableFrom (toClass, fromClass);
	    }
	    return boxing.boxedClass (toClass).isAssignableFrom (fromClass);
	}
	if (fromClass.isPrimitive ())
	{
	    return toClass.isAssignableFrom (boxing.boxedClass (fromClass));
	}
	return false;
    }

    private boolean isPrimitiveAssignableFrom (final Class<?> toClass, final Class<?> fromClass)
    {
	// No simple solution:
	// https://stackoverflow.com/questions/1704634/simple-way-to-get-wrapper-class-type-in-java
	if (toClass == int.class)
	{
	    if (fromClass == int.class || fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == boolean.class)
	{
	    if (fromClass == boolean.class)
	    {
		return true;
	    }
	}
	else if (toClass == double.class)
	{
	    if (fromClass == double.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class
	        || fromClass == float.class || fromClass == long.class)
	    {
		return true;
	    }
	}
	else if (toClass == long.class)
	{
	    if (fromClass == long.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == char.class)
	{
	    if (fromClass == char.class)
	    {
		return true;
	    }
	}
	else if (toClass == byte.class)
	{
	    if (fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == short.class)
	{
	    if (fromClass == short.class || fromClass == byte.class)
	    {
		return true;
	    }
	}
	else if (toClass == float.class)
	{
	    if (fromClass == float.class || fromClass == int.class || fromClass == short.class || fromClass == byte.class
	        || fromClass == float.class || fromClass == long.class)
	    {
		return true;
	    }
	}
	return false;
    }

    public Object apply (final List<Object> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	if (!valid)
	{
	    throw new IllegalAccessException ("Method overload has been replaced");
	}

	return invoke.apply (method, object, arguments);
    }

    /**
     * Return the method signature, as used by the INVOKEMETHOD bytecode instruction.
     */
    public String getSignature ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ('(');
	for (final Class<?> param : method.getParameterTypes ())
	{
	    final Type type = Type.getType (param);
	    buffer.append (type.getDescriptor ());
	}
	buffer.append (')');
	buffer.append (Type.getType (method.getReturnType ()).getDescriptor ());
	return buffer.toString ();
    }

    /**
     * Get the method signature, for the purpose of selecting an overloaded method. This only
     * includes the parameter types, since the method name might be created by the compiler and not
     * match the actual function name used to reference this definition.
     * </p>
     * In the Java programming language, a method signature is the method name and the number, type
     * and order of its parameters. Return types and thrown exceptions are not considered to be a
     * part of the method signature. Type signature - Wikipedia
     *
     * @see https://en.wikipedia.org/wiki/Type_signature
     */
    public String getArgumentSignature ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ('(');
	for (final Class<?> param : method.getParameterTypes ())
	{
	    final Type type = Type.getType (param);
	    buffer.append (type.getDescriptor ());
	}
	buffer.append (')');
	return buffer.toString ();
    }

    // public boolean shadows (final ObjectMethod other)
    // {
    // if (minimumArgCount > other.minimumArgCount)
    // {
    // return false;
    // }
    // final Class<?>[] myTypes = method.getParameterTypes ();
    // final Class<?>[] otherTypes = other.method.getParameterTypes ();
    // for (int i = 0; i < myTypes.length; i++)
    // {
    // final Class<?> mt = myTypes[i];
    // final Class<?> ot = otherTypes[i];
    // if (isAssignableFrom (mt, ot))
    // {
    // Unfinished and not used
    // }
    // }
    // return true;
    // }

    @Override
    public MultiMap<String, Object> getDescriberValues (final Object target)
    {
	final MultiMap<String, Object> result = new MultiMap<String, Object> ();
	result.put ("Object", object);
	result.put ("Method", method);
	if (documentation != null && !documentation.isEmpty ())
	{
	    result.put ("Documentation", documentation);
	}
	if (source != null)
	{
	    result.put ("Source", source);
	}
	if (cn != null)
	{
	    result.put ("Class node", cn);
	    for (final MethodNode mn : cn.methods)
	    {
		if (mn.name.equals (method.getName ()))
		{

		    result.put ("Method node", mn);
		}
	    }
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (object);
	buffer.append (" ");
	buffer.append (method);
	buffer.append (">");
	return buffer.toString ();
    }
}
