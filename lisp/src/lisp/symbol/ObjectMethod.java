
package lisp.symbol;

import java.lang.reflect.Method;
import java.util.*;

import org.objectweb.asm.Type;

import lisp.Describer;

public class ObjectMethod implements Describer
{
    final Object object;
    final Method method;
    final String documentation;

    ObjectMethod (final Object object, final Method method, final String documentation)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
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
     * Direct calls to methods produce bad results when there are non-object parameters. Use this
     * method to filter out those cases until it is debugged.
     *
     * @return True if this is a simple method call that only involves objects.
     */
    public boolean isObjectOnly ()
    {
	for (final Class<?> type : method.getParameterTypes ())
	{
	    if (!type.equals (Object.class))
	    {
		return false;
	    }
	}
	return method.getReturnType ().equals (Object.class);
    }

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

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Object", object);
	result.put ("Method", method);
	if (documentation != null)
	{
	    result.put ("Documentation", documentation);
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
