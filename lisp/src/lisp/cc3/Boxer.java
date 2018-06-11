
package lisp.cc3;

import java.util.*;

import org.objectweb.asm.Type;

public class Boxer
{
    public static final String CLDESC = "Ljava/lang/Class;";

    public static final Type BYTE_TYPE = Type.getObjectType ("java/lang/Byte");

    public static final Type BOOLEAN_TYPE = Type.getObjectType ("java/lang/Boolean");

    public static final Type SHORT_TYPE = Type.getObjectType ("java/lang/Short");

    public static final Type CHARACTER_TYPE = Type.getObjectType ("java/lang/Character");

    public static final Type INTEGER_TYPE = Type.getObjectType ("java/lang/Integer");

    public static final Type FLOAT_TYPE = Type.getObjectType ("java/lang/Float");

    public static final Type LONG_TYPE = Type.getObjectType ("java/lang/Long");

    public static final Type DOUBLE_TYPE = Type.getObjectType ("java/lang/Double");

    public static final Type NUMBER_TYPE = Type.getObjectType ("java/lang/Number");

    public static final Type OBJECT_TYPE = Type.getObjectType ("java/lang/Object");

    // protected static final Method BOOLEAN_VALUE = Method.getMethod ("boolean booleanValue()");
    //
    // protected static final Method CHAR_VALUE = Method.getMethod ("char charValue()");
    //
    // protected static final Method INT_VALUE = Method.getMethod ("int intValue()");
    //
    // protected static final Method FLOAT_VALUE = Method.getMethod ("float floatValue()");
    //
    // protected static final Method LONG_VALUE = Method.getMethod ("long longValue()");
    //
    // protected static final Method DOUBLE_VALUE = Method.getMethod ("double doubleValue()");

    private static Class<?>[][] BOXED_PRIMITIVE_CLASSES =
	{
	 {byte.class, Byte.class},
	 {boolean.class, Boolean.class},
	 {short.class, Short.class},
	 {char.class, Character.class},
	 {int.class, Integer.class},
	 {long.class, Long.class},
	 {float.class, Float.class},
	 {double.class, Double.class}};

    // private static Type[][] BOXED_PRIMITIVES =
    // {
    // {BYTE_TYPE, Type.BYTE_TYPE},
    // {BOOLEAN_TYPE, Type.BOOLEAN_TYPE},
    // {SHORT_TYPE, Type.SHORT_TYPE},
    // {CHARACTER_TYPE, Type.CHAR_TYPE},
    // {INTEGER_TYPE, Type.INT_TYPE},
    // {FLOAT_TYPE, Type.FLOAT_TYPE},
    // {LONG_TYPE, Type.LONG_TYPE},
    // {DOUBLE_TYPE, Type.DOUBLE_TYPE}};

    private final Map<Class<?>, Class<?>> primitive2boxed = new HashMap<Class<?>, Class<?>> ();
    private final Map<Class<?>, Class<?>> boxed2primitive = new HashMap<Class<?>, Class<?>> ();

    private final Map<Type, Type> primitive2boxedType = new HashMap<Type, Type> ();
    private final Map<Type, Type> boxed2primitiveType = new HashMap<Type, Type> ();

    public Boxer ()
    {
	for (final Class<?>[] entry : BOXED_PRIMITIVE_CLASSES)
	{
	    final Class<?> primitiveClass = entry[0];
	    final Class<?> boxedClass = entry[1];
	    primitive2boxed.put (primitiveClass, boxedClass);
	    boxed2primitive.put (boxedClass, primitiveClass);
	    final Type primitiveType = Type.getType (primitiveClass);
	    final Type boxedType = Type.getType (boxedClass);
	    primitive2boxedType.put (primitiveType, boxedType);
	    boxed2primitiveType.put (boxedType, primitiveType);
	}
    }

    public Class<?> getBoxedClass (final Class<?> primitiveClass)
    {
	return primitive2boxed.get (primitiveClass);
    }

    public Class<?> getUnboxedClass (final Class<?> boxedClass)
    {
	return boxed2primitive.get (boxedClass);
    }

    public Type getBoxedType (final Type type)
    {
	return primitive2boxedType.get (type);
	// for (final Type[] clause : BOXED_PRIMITIVES)
	// {
	// if (clause[1] == type)
	// {
	// return clause[0];
	// }
	// }
	// return null;
    }

    public Type getUnboxedType (final Type type)
    {
	return boxed2primitiveType.get (type);
	// for (final Type[] clause : BOXED_PRIMITIVES)
	// {
	// if (clause[0] == type)
	// {
	// return clause[1];
	// }
	// }
	// return null;
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
