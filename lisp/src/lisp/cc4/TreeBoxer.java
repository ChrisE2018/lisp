
package lisp.cc4;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.InsnList;

import lisp.asm.instructions.MethodInsnNode;

public class TreeBoxer implements Opcodes
{
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

    private final Map<Class<?>, Class<?>> primitive2boxed = new HashMap<Class<?>, Class<?>> ();
    private final Map<Class<?>, Class<?>> boxed2primitive = new HashMap<Class<?>, Class<?>> ();

    private final Map<Type, Type> primitive2boxedType = new HashMap<Type, Type> ();
    private final Map<Type, Type> boxed2primitiveType = new HashMap<Type, Type> ();

    public TreeBoxer ()
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
    }

    public Type getUnboxedType (final Type type)
    {
	return boxed2primitiveType.get (type);
    }

    public void box (final InsnList il, final Type primitiveType)
    {
	switch (primitiveType.getSort ())
	{
	    case Type.BOOLEAN:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false));
		break;
	    case Type.BYTE:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false));
		break;
	    case Type.CHAR:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false));
		break;
	    case Type.SHORT:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false));
		break;
	    case Type.INT:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false));
		break;
	    case Type.FLOAT:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false));
		break;
	    case Type.LONG:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false));
		break;
	    case Type.DOUBLE:
		il.add (new MethodInsnNode (INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false));
		break;
	}
    }

    public void unbox (final InsnList il, final Type primitiveType)
    {
	switch (primitiveType.getSort ())
	{
	    case Type.BOOLEAN:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
		break;
	    case Type.BYTE:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Byte", "byteValue", "()B", false));
		break;
	    case Type.CHAR:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Character", "charValue", "()C", false));
		break;
	    case Type.SHORT:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Short", "shortValue", "()S", false));
		break;
	    case Type.INT:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false));
		break;
	    case Type.FLOAT:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Float", "floatValue", "()F", false));
		break;
	    case Type.LONG:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Long", "longValue", "()J", false));
		break;
	    case Type.DOUBLE:
		il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Double", "doubleValue", "()D", false));
		break;
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
