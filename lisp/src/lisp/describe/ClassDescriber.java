
package lisp.describe;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;

import lisp.lang.Describer;
import lisp.util.MultiMap;

public class ClassDescriber implements Describer
{
    /**
     * Append to a map describing an object. The return value is intended to be used by a debugger
     * to print an object decomposition.
     *
     * @param result The map to add entries to.
     * @param target The object to describe.
     */
    public void getDescriberValues (final MultiMap<String, Object> result, final Object target)
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
	// for (final Field field : cls.getDeclaredFields ())
	// {
	// result.put ("Declared Field", field);
	// }
	for (final Constructor<?> method : cls.getConstructors ())
	{
	    result.put ("Public Constructor", method);
	}
	// for (final Constructor<?> method : cls.getDeclaredConstructors ())
	// {
	// result.put ("Declared Constructor", method);
	// }
	for (final Method method : cls.getMethods ())
	{
	    if (Modifier.isStatic (method.getModifiers ()))
	    {
		result.put ("Public Static Method", method);
	    }
	}
	for (final Method method : cls.getMethods ())
	{
	    if (!Modifier.isStatic (method.getModifiers ()))
	    {
		result.put ("Public Method", method);
	    }
	}
	// for (final Method method : cls.getDeclaredMethods ())
	// {
	// result.put ("Declared Method", method);
	// }
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
