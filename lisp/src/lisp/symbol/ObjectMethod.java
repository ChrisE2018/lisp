
package lisp.symbol;

import java.lang.reflect.Method;
import java.util.*;

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

    String getMethodName ()
    {
	return method.getName ();
    }

    boolean isVarArgs ()
    {
	return method.isVarArgs ();
    }

    Class<?>[] getParameterTypes ()
    {
	return method.getParameterTypes ();
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
