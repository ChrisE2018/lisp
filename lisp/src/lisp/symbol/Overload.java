/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import lisp.Describer;
import lisp.eval.Invoke;
import lisp.util.MultiMap;

/**
 * Combination of Object and Method that can be called as an overloaded function. This should be
 * renamed to Overload.
 *
 * @author cre
 */
public class Overload implements Describer
{
    private static Invoke invoke = new Invoke ();
    private static Assignable assignable = new Assignable ();

    final Object object;
    final Method method;
    final String documentation;

    /** Place to store the lisp source, if available. */
    final Object source;

    /** Place to store the ASM bytecode, if available. */
    final ClassNode cn;

    /**
     * If this method overload is replaced, set this to false. Compiled code can be setup to check
     * this flag and repair a compiled call to this method if it changes.
     */
    boolean valid = true;

    Overload (final Object object, final Method method, final String documentation, final Object source, final ClassNode cn)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
	this.source = source;
	this.cn = cn;
    }

    Overload (final Object object, final Method method, final String documentation)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
	source = null;
	cn = null;
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
     * Determine if this method overload is better than another viable candidate.
     *
     * @param otherMethod Another overload option that is known to be applicable to an argument set.
     * @return True if this method overload should be used in preference to the other overload.
     */
    public boolean isBetterThan (final Overload otherMethod)
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
