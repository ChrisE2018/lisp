
package lisp.eval;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;

import org.objectweb.asm.tree.*;

import lisp.*;
import lisp.Package;
import lisp.asm.instructions.AccessKeywords;

public class DescribePrimitives extends Definer
{
    @DefineLisp
    public Object describe (final Object arg)
    {
	System.out.printf ("Describe: %s \n", arg);
	if (arg != null)
	{
	    final Describer d = getDescriber (arg);
	    if (d != null)
	    {
		describe (d, arg);
	    }
	    else
	    {
		System.out.printf ("Class: %s \n", arg.getClass ().getCanonicalName ());
	    }
	}
	return Boolean.FALSE;
    }

    private Describer getDescriber (final Object arg)
    {
	if (arg == null)
	{
	    return new ObjectDescriber ();
	}
	if (arg instanceof Describer)
	{
	    return (Describer)arg;
	}
	if (arg instanceof List)
	{
	    return new LispList ();
	}
	if (arg instanceof Class)
	{
	    return new ClassDescriber ();
	}
	else if (arg instanceof ClassNode)
	{
	    return new ClassNodeDescriber ();
	}
	else if (arg instanceof MethodNode)
	{
	    return new MethodNodeDescriber ();
	}
	else if (arg instanceof InsnList)
	{
	    return new InsnListDescriber ();
	}
	else if (arg instanceof AbstractInsnNode)
	{
	    return new AbstractInsnNodeDescriber ();
	}
	else
	{
	    final Class<?> cls = arg.getClass ();
	    if (cls.isArray ())
	    {
		return new ArrayDescriber ();
	    }
	    else
	    {
		return new ObjectDescriber ();
	    }
	}
    }

    private void describe (final Describer d, final Object arg)
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	int index = 0;
	final Map<String, Object> description = d.getDescriberValues (arg);
	for (final Entry<String, Object> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    ++index;
	    final String key = entry.getKey ();
	    final Object value = entry.getValue ();
	    final Describer valueDescriber = getDescriber (value);
	    final String valueString = (valueDescriber == null) ? value.toString () : valueDescriber.getDescriberString (value);
	    final String doc = d.getDescriberDocumentation (arg, key);
	    final Symbol symbol = pkg.internSymbol (String.format ("d%d", index));
	    symbol.setValue (value);
	    final String type = value == null ? "" : "(" + value.getClass ().getSimpleName () + ") ";
	    if (doc != null)
	    {
		System.out.printf ("%3s %30s: %-25s %s\n", symbol, type + key, valueString, doc);
	    }
	    else
	    {
		System.out.printf ("%3s %30s: %-25s\n", symbol, type + key, valueString);
	    }
	}
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

class ObjectDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final Class<?> cls = target.getClass ();
	result.put ("Class", cls);
	result.put ("Hashcode", target.hashCode ());
	final Method[] methods = cls.getMethods ();
	for (final Method method : methods)
	{
	    if (method.getParameterTypes ().length == 0)
	    {
		final String methodName = method.getName ();
		if (methodName.startsWith ("get"))
		{
		    try
		    {
			final Object value = method.invoke (target);
			result.put (methodName.substring (3), value);
		    }
		    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
		    {
			e.printStackTrace ();
		    }
		}
		if (methodName.startsWith ("is"))
		{
		    try
		    {
			final Object value = method.invoke (target);
			result.put (methodName.substring (2), value);
		    }
		    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
		    {
			e.printStackTrace ();
		    }
		}
	    }
	}
    }
}

class ClassDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final Class<?> cls = (Class<?>)target;
	result.put ("Class", cls.getCanonicalName ());
	result.put ("Loader", cls.getClassLoader ());
	result.put ("Package", cls.getPackage ());
	if (cls.isInterface ())
	{
	    result.put ("Interface", true);
	}
	if (cls.isArray ())
	{
	    result.put ("Array", true);
	    result.put ("Component Type", cls.getComponentType ());
	}
	if (cls.isPrimitive ())
	{
	    result.put ("Primitive", true);
	}
	if (cls.isAnnotation ())
	{
	    result.put ("Annotation", true);
	}
	if (cls.isSynthetic ())
	{
	    result.put ("Synthetic", true);
	}
	if (cls.isEnum ())
	{
	    result.put ("Enum", true);
	    final Object[] vals = cls.getEnumConstants ();
	    for (int i = 0; i < vals.length; i++)
	    {
		result.put (String.valueOf (i), vals[i]);
	    }
	}
	result.put ("Extends", cls.getSuperclass ());
	for (final Class<?> intf : cls.getInterfaces ())
	{
	    result.put ("Interface", intf);
	}
	for (final Annotation a : cls.getAnnotations ())
	{
	    result.put ("Annotation", a);
	}
	for (final Field field : cls.getFields ())
	{
	    result.put ("Public Field", field);
	}
	for (final Field field : cls.getDeclaredFields ())
	{
	    result.put ("Declared Field", field);
	}
	for (final Constructor<?> method : cls.getConstructors ())
	{
	    result.put ("Public Constructor", method);
	}
	for (final Constructor<?> method : cls.getDeclaredConstructors ())
	{
	    result.put ("Declared Constructor", method);
	}
	for (final Method method : cls.getMethods ())
	{
	    result.put ("Public Method", method);
	}
	for (final Method method : cls.getDeclaredMethods ())
	{
	    result.put ("Declared Method", method);
	}
    }
}

class ArrayDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	return "[" + getArrayString (target, 50) + "]";
    }

    private String getArrayString (final Object array, final int maxLength)
    {
	final StringBuilder buffer = new StringBuilder ();
	final int length = Array.getLength (array);
	for (int i = 0; i < length; i++)
	{
	    if (i > 0)
	    {
		buffer.append (", ");
	    }
	    if (buffer.length () > maxLength)
	    {
		buffer.append ("...");
		return buffer.toString ();
	    }
	    buffer.append (Array.get (array, i));
	}
	return buffer.toString ();
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object array)
    {
	final int length = Array.getLength (array);
	for (int i = 0; i < length; i++)
	{
	    final Object element = Array.get (array, i);
	    result.put (String.valueOf (i), element);
	}
    }
}

class ClassNodeDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final ClassNode cn = (ClassNode)target;
	result.put ("Version", cn.version);
	result.put ("Access", AccessKeywords.find (cn.access));
	result.put ("Name", cn.name);
	result.put ("Signature", cn.signature);
	for (final FieldNode field : cn.fields)
	{
	    result.put ("Field", field);
	}
	final List<MethodNode> methods = cn.methods;
	for (final MethodNode method : methods)
	{
	    result.put ("Method", method);
	}
    }
}

class MethodNodeDescriber implements Describer
{

    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof MethodNode)
	{
	    final MethodNode mn = (MethodNode)target;
	    return AccessKeywords.find (mn.access) + " method " + mn.name;
	}
	else
	{
	    return target.toString ();
	}
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final MethodNode mn = (MethodNode)target;
	result.put ("Access", AccessKeywords.find (mn.access));
	result.put ("Name", mn.name);
	result.put ("Description", mn.desc);
	if (mn.signature != null)
	{
	    result.put ("Signature", mn.signature);
	}
	result.put ("Exceptions", mn.exceptions);
	final StringBuilder buffer = new StringBuilder ();
	final List<ParameterNode> parameters = mn.parameters;
	if (parameters != null)
	{
	    for (final ParameterNode p : mn.parameters)
	    {
		if (buffer.length () == 0)
		{
		    buffer.append (", ");
		}
		buffer.append (p.name);
	    }
	    result.put ("Parameters", buffer.toString ());
	}
	result.put ("Instructions", mn.instructions);
    }
}

class InsnListDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof InsnList)
	{
	    return ((InsnList)target).size () + " instructions";
	}
	else
	{
	    return target.toString ();
	}
    }

    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final Map<String, Object> result, final Object target)
    {
	final InsnList il = (InsnList)target;
	for (int i = 0; i < il.size (); i++)
	{
	    final AbstractInsnNode instruction = il.get (i);
	    result.put (String.valueOf (i), instruction);
	}
    }
}

class AbstractInsnNodeDescriber implements Describer
{
    /** Convert an object to a string for printing. */
    public String getDescriberString (final Object target)
    {
	if (target instanceof LabelNode)
	{
	    return target.toString ();
	}
	if (target instanceof AbstractInsnNode)
	{
	    return "      " + target.toString ();
	}
	return target.toString ();
    }
}
