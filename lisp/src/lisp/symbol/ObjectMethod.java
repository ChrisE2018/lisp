
package lisp.symbol;

import java.lang.reflect.Method;

import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import lisp.Describer;
import lisp.util.MultiMap;

public class ObjectMethod implements Describer
{
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

    ObjectMethod (final Object object, final Method method, final String documentation, final Object source, final ClassNode cn)
    {
	this.object = object;
	this.method = method;
	this.documentation = documentation;
	this.source = source;
	this.cn = cn;
    }

    ObjectMethod (final Object object, final Method method, final String documentation)
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
