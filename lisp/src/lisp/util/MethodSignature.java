
package lisp.util;

import java.lang.reflect.*;

import org.objectweb.asm.Type;

public class MethodSignature
{
    /**
     * Return the method signature, as used by the INVOKEMETHOD bytecode instruction.
     */
    public String getMethodSignature (final Method method)
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
     * Return the method signature, as used by the INVOKEMETHOD bytecode instruction.
     */
    public String getMethodSignature (final Constructor<?> method)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ('(');
	for (final Class<?> param : method.getParameterTypes ())
	{
	    final Type type = Type.getType (param);
	    buffer.append (type.getDescriptor ());
	}
	buffer.append (')');
	buffer.append ('V');
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
    public String getArgumentSignature (final Method method)
    {
	return getArgumentSignature (method.getParameterTypes ());
    }

    public String getArgumentSignature (final Constructor<?> method)
    {
	return getArgumentSignature (method.getParameterTypes ());
    }

    public String getArgumentSignature (final Class<?>[] parameters)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ('(');
	for (final Class<?> param : parameters)
	{
	    final Type type = Type.getType (param);
	    buffer.append (type.getDescriptor ());
	}
	buffer.append (')');
	return buffer.toString ();
    }
}
