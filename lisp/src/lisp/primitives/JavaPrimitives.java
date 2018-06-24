
package lisp.primitives;

import java.lang.reflect.Field;

import lisp.eval.*;

public class JavaPrimitives extends Definer
{
    @DefineLisp
    public Object field (final Class<?> cls, final String name)
            throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException
    {
	final Field field = cls.getField (name);
	return field.get (cls);
    }

    @DefineLisp
    public Object field (final Object target, final String name)
            throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException
    {
	return field (target.getClass (), name);
    }
}
