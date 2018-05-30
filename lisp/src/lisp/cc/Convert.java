
package lisp.cc;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

public class Convert implements Opcodes
{
    private static final Boxer boxer = new Boxer ();

    /**
     * Convert the top element of the stack from one type to another. This is the only public method
     * in this gigantic class. This method will generate code to convert from one class to another
     * class. The generated code will take advantage of provided type information to implement the
     * best conversion possible. If the types are known at compile time, the primitive types will be
     * converted directly. If not known at compile time, a series of type checks will be put into
     * the code to do the right thing.
     *
     * @param mv The bytecode generator
     * @param fromType The current type of the top stack element.
     * @param toType The required type of the top stack element. We must convert to this type (or a
     *            subtype).
     */
    public void convert (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	final int toSort = toType.getSort ();
	if (fromSort == toSort)
	{
	    if (fromSort != Type.ARRAY && fromSort != Type.OBJECT && fromSort != Type.METHOD)
	    {
		// Same primitive types are good already
		return;
	    }
	}
	// Check for simple unboxing
	if (fromType.equals (boxer.getBoxedType (toType)))
	{
	    mv.unbox (fromType);
	    return;
	}
	// Check for simple boxing
	if (toType.equals (boxer.getBoxedType (fromType)))
	{
	    mv.box (fromType);
	    return;
	}
	switch (toSort)
	{
	    case Type.VOID:
		convert2void (mv, fromClass);
		return;
	    case Type.BOOLEAN:
		if (fromSort == Type.OBJECT)
		{
		    if (fromClass.equals (Boolean.class))
		    {
			mv.unbox (fromType);
			return;
		    }
		    else
		    {
			coerceBoolean (mv);
		    }
		    return;
		}
		cantConvert (fromClass, toClass);
		break;

	    case Type.CHAR:
	    {
		convert2char (mv, fromClass, toClass);
		return;
	    }
	    case Type.BYTE:
	    {
		convert2byte (mv, fromClass, toClass);

		return;
	    }
	    case Type.SHORT:
	    {
		convert2short (mv, fromClass, toClass);
		return;
	    }
	    case Type.INT:
	    {
		convert2int (mv, fromClass, toClass);
		return;
	    }
	    case Type.LONG:
	    {
		convert2long (mv, fromClass, toClass);
		return;
	    }
	    case Type.FLOAT:
	    {
		convert2float (mv, fromClass, toClass);
		return;
	    }
	    case Type.DOUBLE:
	    {
		convert2double (mv, fromClass, toClass);
		return;
	    }
	    case Type.OBJECT:
	    {
		// Special case for String, Symbol
		convert2Object (mv, fromClass, toClass);
		return;
	    }
	    case Type.ARRAY:
	    case Type.METHOD:
	    default:
		cantConvert (fromClass, toClass);
	}
	cantConvert (fromClass, toClass);
    }

    private void throwException (final GeneratorAdapter mv, final String internalName)
    {
	mv.visitTypeInsn (NEW, internalName);
	mv.visitInsn (DUP);
	mv.visitMethodInsn (INVOKESPECIAL, internalName, "<init>", "()V", false);
	mv.visitInsn (ATHROW);
    }

    private void throwException (final GeneratorAdapter mv, final String internalName, final String format, final Object... args)
    {
	mv.visitTypeInsn (NEW, internalName);
	mv.visitInsn (DUP);
	final String message = String.format (format, args);
	mv.visitLdcInsn (message);
	mv.visitMethodInsn (INVOKESPECIAL, internalName, "<init>", "(Ljava/lang/String;)V", false);
	mv.visitInsn (ATHROW);
    }

    private void cantConvert (final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	throw new Error ("Can't convert from " + fromClass + "(" + fromType + ") to " + toClass + "(" + toType + ")");
    }

    /**
     * Convert value on top of the stack from a Boolean to a boolean. This is used as a last resort
     * when the return type must be boolean and there is no better way to get there. If the top of
     * the stack is not a Boolean, the result left on the stack is always true.
     */
    public void coerceBoolean (final GeneratorAdapter mv)
    {
	// (define boolean:foo () true)
	// (define boolean:foo (x) x)
	mv.visitInsn (DUP);
	final Label l1 = new Label ();
	// [TODO] There is a private method in GeneratorAdaptor that we need to steal: getBoxedType
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	mv.visitJumpInsn (IFNE, l1);
	mv.visitInsn (POP);
	mv.visitLdcInsn (true);
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);
	mv.visitLabel (l1);
	mv.unbox (Type.BOOLEAN_TYPE);
	// mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	// mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	mv.visitLabel (l2);
    }

    private void convertByte2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Byte");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.BYTE_TYPE);
    }

    private void convertChar2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Character");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.CHAR_TYPE);
    }

    private void convertShort2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Short");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.SHORT_TYPE);
    }

    private void convertInt2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Integer");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.INT_TYPE);
    }

    private void convertLong2long (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Long");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.LONG_TYPE);
    }

    private void convertFloat2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.FLOAT_TYPE);
    }

    private void convertFloat2double (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.FLOAT_TYPE);
	mv.visitInsn (F2D);
    }

    private void convertDouble2double (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Double");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.DOUBLE_TYPE);
    }

    private void convert2void (final GeneratorAdapter mv, final Class<?> fromClass)
    {
	final Type fromType = Type.getType (fromClass);
	final int fromSize = fromType.getSize ();

	// Void desired. Just pop the stack.
	if (fromSize == 0)
	{
	    // Good already
	}
	else if (fromSize == 1)
	{
	    mv.visitInsn (POP);
	}
	else if (fromSize == 2)
	{
	    mv.visitInsn (POP2);
	}
    }

    // Char
    // Byte
    private void convert2char (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.CHAR || fromSort == Type.BYTE)
	{
	    // OK already
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class))
	{
	    mv.unbox (toType);
	    return;
	}
	final Label l0 = new Label (); // Good
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
	// Succeed
    }

    private void convert2byte (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.CHAR || fromSort == Type.BYTE)
	{
	    // OK already
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class))
	{
	    mv.unbox (toType);
	    return;
	}
	final Label l0 = new Label (); // Good
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
	// Succeed
    }

    private void convert2short (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final int fromSort = fromType.getSort ();

	if (fromSort == Type.CHAR || fromSort == Type.BYTE || fromSort == Type.SHORT)
	{
	    // OK already
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class))
	{
	    mv.unbox (boxer.getUnboxedType (fromType));
	    return;
	}

	final Label l0 = new Label (); // Good
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	convertShort2int (mv, l3);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l3);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
    }

    private void convert2int (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.CHAR || fromSort == Type.BYTE || fromSort == Type.SHORT || fromSort == Type.INT)
	{
	    // OK already
	    return;
	}

	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (boxer.getUnboxedType (fromType));
	    return;
	}

	final Label l0 = new Label (); // Good
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	convertShort2int (mv, l3);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l3);
	convertInt2int (mv, l4);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l4);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
    }

    private void convert2long (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.LONG)
	{
	    // OK already
	    return;
	}
	if (fromSort == Type.CHAR || fromSort == Type.BYTE || fromSort == Type.SHORT || fromSort == Type.INT)
	{
	    // Extend
	    mv.visitInsn (I2L);
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (I2L);
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    mv.unbox (toType);
	    return;
	}
	// Last choice: convert and check dynamically
	final Label l0 = new Label (); // got to int
	final Label l00 = new Label (); // got to long
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	convertShort2int (mv, l3);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l3);
	convertInt2int (mv, l4);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l4);
	convertLong2long (mv, l5);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l5);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
	mv.visitInsn (I2L);
	mv.visitLabel (l00);
    }

    private void convert2float (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();

	if (fromSort == Type.FLOAT)
	{
	    // OK already
	    return;
	}
	if (fromClass.equals (Float.class))
	{
	    mv.unbox (boxer.getUnboxedType (fromType));
	    return;
	}

	final Label l0 = new Label (); // got to int
	final Label l00 = new Label (); // got to float
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l2);
	convertShort2int (mv, l3);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l3);
	convertInt2int (mv, l4);
	mv.visitJumpInsn (GOTO, l0);
	mv.visitLabel (l4);
	convertFloat2float (mv, l5);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l5);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l0);
	mv.visitInsn (I2F);
	mv.visitLabel (l00);
    }

    private void convert2double (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.DOUBLE)
	{
	    // OK already
	    return;
	}
	if (fromSort == Type.FLOAT)
	{
	    // Extend
	    mv.visitInsn (F2D);
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (I2D);
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (L2D);
	    return;
	}
	if (fromClass.equals (Float.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (F2D);
	    return;
	}
	if (fromClass.equals (Double.class))
	{
	    mv.unbox (toType);
	    return;
	}
	// Last choice: convert and check dynamically
	final Label lint = new Label (); // got to int
	final Label llong = new Label (); // got to long
	final Label lfloat = new Label (); // got to double
	final Label ldouble = new Label (); // got to double
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	final Label l6 = new Label ();
	final Label l7 = new Label ();
	convertByte2int (mv, l1);
	mv.visitJumpInsn (GOTO, lint);
	mv.visitLabel (l1);
	convertChar2int (mv, l2);
	mv.visitJumpInsn (GOTO, lint);
	mv.visitLabel (l2);
	convertShort2int (mv, l3);
	mv.visitJumpInsn (GOTO, lint);
	mv.visitLabel (l3);
	convertInt2int (mv, l4);
	mv.visitJumpInsn (GOTO, lint);
	mv.visitLabel (l4);
	convertLong2long (mv, l5);
	mv.visitJumpInsn (GOTO, llong);
	mv.visitLabel (l5);

	convertFloat2float (mv, l6);
	mv.visitJumpInsn (GOTO, lfloat);
	mv.visitLabel (l6);

	convertDouble2double (mv, l7);
	mv.visitJumpInsn (GOTO, ldouble);
	mv.visitLabel (l7);

	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);

	mv.visitLabel (lint);
	mv.visitInsn (I2D);
	mv.visitJumpInsn (GOTO, ldouble);

	mv.visitLabel (llong);
	mv.visitInsn (L2D);
	mv.visitJumpInsn (GOTO, ldouble);

	mv.visitLabel (lfloat);
	mv.visitInsn (F2D);
	mv.visitLabel (ldouble);
    }

    private void convert2Object (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	// Need to check that the classes are compatible
	if (toClass.isAssignableFrom (fromClass))
	{
	    mv.visitTypeInsn (CHECKCAST, toType.getInternalName ());
	    return;
	}
	final Type boxedFromType = boxer.getBoxedType (fromType);
	if (toType.equals (boxedFromType))
	{
	    mv.box (fromType);
	    return;
	}
	if (toType.equals (Boxer.OBJECT_TYPE))
	{
	    mv.box (fromType);
	    return;
	}
	mv.visitLdcInsn (toType);
	mv.visitInsn (SWAP);
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Class", "isAssignableFrom", "(Ljava/lang/Class;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);
	throwException (mv, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	mv.visitLabel (l1);
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
