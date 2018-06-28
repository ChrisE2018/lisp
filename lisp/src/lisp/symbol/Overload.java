/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.lang.reflect.*;
import java.util.List;

import org.objectweb.asm.tree.*;

import lisp.eval.Invoke;
import lisp.lang.Describer;
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

    /** Optional support object with compiler and analyzer. */
    private LispFunction lispFunction;

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
	if (method.isVarArgs () && !otherMethod.isVarArgs ())
	{
	    return false;
	}
	if (!method.isVarArgs () && otherMethod.isVarArgs ())
	{
	    return true;
	}
	final Class<?>[] myTypes = method.getParameterTypes ();
	final Class<?>[] otherTypes = otherMethod.getParameterTypes ();
	if (myTypes.length > otherTypes.length)
	{
	    // Prefer fixed argument methods to VarArgs methods.
	    return true;
	}
	else if (otherTypes.length > myTypes.length)
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

    public Object apply (final List<?> arguments)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    {
	if (!valid)
	{
	    throw new IllegalAccessException ("Method overload has been replaced");
	}

	return invoke.apply (method, object, arguments);
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
	if (lispFunction != null)
	{
	    result.put ("Function", lispFunction);
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
