
package lisp.cc;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

/**
 * Class to generate conversion code. There are only two public methods in this class:
 * pushDefaultValue and convert.
 */
public class Convert implements Opcodes
{
    private static final Boxer boxer = new Boxer ();

    /**
     * Push a default value of a specified class onto the stack.
     *
     * @param mv GeneratorAdapter to produce code.
     * @param valueClass The value type to return.
     * @param booleanDefault If the value will be a primitive boolean, use this as the default
     *            value.
     */
    public void pushDefaultValue (final GeneratorAdapter mv, final Class<?> valueClass, final boolean booleanDefault)
    {
	if (valueClass != null)
	{
	    if (boolean.class.equals (valueClass))
	    {
		mv.visitLdcInsn (booleanDefault);
	    }
	    else if (byte.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Byte.valueOf ((byte)0));
	    }
	    else if (char.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Character.valueOf ((char)0));
	    }
	    else if (short.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Short.valueOf ((short)0));
	    }
	    else if (int.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Integer.valueOf (0));
	    }
	    else if (long.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Long.valueOf (0));
	    }
	    else if (float.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Float.valueOf (0.0f));
	    }
	    else if (double.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Double.valueOf (0.0));
	    }
	    else
	    {
		mv.visitInsn (ACONST_NULL);
	    }
	}
    }

    /**
     * Convert the top element of the stack from one type to another. This is the one of two public
     * methods in this gigantic class. This method will generate code to convert from one class to
     * another class. The generated code will take advantage of provided type information to
     * implement the best conversion possible. If the types are known at compile time, the primitive
     * types will be converted directly. If not known at compile time, a series of type checks will
     * be put into the code to do the right thing.
     * <p>
     * Supported conversions include all the boxing and unboxing operations for primitive data
     * types.
     * </p>
     * <p>
     * If the toClass is void, then the current data will be popped from the stack. If fromClass is
     * void a default value will be pushed onto the stack.
     * </p>
     *
     * @param mv The bytecode generator
     * @param fromType The current type of the top stack element.
     * @param toType The required type of the top stack element. We must convert to this type (or a
     *            subtype).
     * @param allowNarrowing When true, narrowing conversions will be generated if required.
     *            Otherwise narrowing throws and error.
     * @param liberalTruth When set, any non-boolean result is accepted as true. Otherwise, boolean
     *            testing requires strictly boolean values.
     */
    public void convert (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing, final boolean liberalTruth)
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
	if (fromSort == Type.VOID)
	{
	    pushDefaultValue (mv, toClass, false);
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
		convert2char (mv, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.BYTE:
	    {
		convert2byte (mv, fromClass, toClass, allowNarrowing);

		return;
	    }
	    case Type.SHORT:
	    {
		convert2short (mv, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.INT:
	    {
		convert2int (mv, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.LONG:
	    {
		convert2long (mv, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.FLOAT:
	    {
		convert2float (mv, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.DOUBLE:
	    {
		convert2double (mv, fromClass, toClass);
		return;
	    }
	    case Type.OBJECT:
	    {
		// [TODO] Special case for String, Symbol?
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

    // private void throwException (final GeneratorAdapter mv, final String internalName)
    // {
    // mv.visitTypeInsn (NEW, internalName);
    // mv.visitInsn (DUP);
    // mv.visitMethodInsn (INVOKESPECIAL, internalName, "<init>", "()V", false);
    // mv.visitInsn (ATHROW);
    // }

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
     *
     * @Deprecated This needs to be checked to verify that liberalTruth and allowNarrowing are
     *             handled correctly.
     */
    @Deprecated
    public void coerceBoolean (final GeneratorAdapter mv)
    {
	// (define boolean:foo () true)
	// (define boolean:foo (x) x)
	mv.visitInsn (DUP);
	final Label l1 = new Label ();
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	mv.visitJumpInsn (IFNE, l1);
	mv.visitInsn (POP);
	mv.visitLdcInsn (true);
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);
	mv.visitLabel (l1);
	mv.unbox (Type.BOOLEAN_TYPE);
	mv.visitLabel (l2);
    }

    /**
     * If there is a Byte on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Byte.
     */
    private void convertByte2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Byte");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.BYTE_TYPE);
    }

    /**
     * If there is a Character on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Character.
     */
    private void convertChar2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Character");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.CHAR_TYPE);
    }

    /**
     * If there is a Short on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Short.
     */
    private void convertShort2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Short");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.SHORT_TYPE);
    }

    /**
     * If there is a Integer on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Integer.
     */
    private void convertInt2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Integer");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.INT_TYPE);
    }

    /**
     * If there is a Long on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Long");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.LONG_TYPE);
	mv.visitInsn (L2I);
    }

    /**
     * If there is a Float on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.FLOAT_TYPE);
	mv.visitInsn (F2I);
    }

    /**
     * If there is a Double on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2int (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Double");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.DOUBLE_TYPE);
	mv.visitInsn (D2I);
    }

    /**
     * If there is a Long on the stack, convert it to an long. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2long (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Long");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.LONG_TYPE);
    }

    /**
     * If there is a Float on the stack, convert it to an long. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2long (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.FLOAT_TYPE);
	mv.visitInsn (F2L);
    }

    /**
     * If there is a Double on the stack, convert it to an long. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2long (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Double");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.DOUBLE_TYPE);
	mv.visitInsn (D2L);
    }

    /**
     * If there is a Byte on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Byte.
     */
    private void convertByte2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Byte");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.BYTE_TYPE);
	mv.visitInsn (I2F);
    }

    /**
     * If there is a Character on the stack, convert it to an float. Otherwise, leave the top value
     * on the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Character.
     */
    private void convertCharacter2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Character");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.CHAR_TYPE);
	mv.visitInsn (I2F);
    }

    /**
     * If there is a Short on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Short.
     */
    private void convertShort2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Short");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.SHORT_TYPE);
	mv.visitInsn (I2F);
    }

    /**
     * If there is a Integer on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Integer.
     */
    private void convertInteger2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Integer");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.INT_TYPE);
	mv.visitInsn (I2F);
    }

    /**
     * If there is a Float on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.FLOAT_TYPE);
    }

    /**
     * If there is a Long on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Long");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.LONG_TYPE);
	mv.visitInsn (L2F);
    }

    /**
     * If there is a Double on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2float (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Double");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.DOUBLE_TYPE);
	mv.visitInsn (D2F);
    }

    // /**
    // * If there is a Float on the stack, convert it to a double. Otherwise, leave the top value on
    // * the stack alone and jump to the label otherwise.
    // *
    // * @param mv Bytecode generator.
    // * @param otherwise Label to jump to if the stack does not have a Float.
    // */
    // private void convertFloat2double (final GeneratorAdapter mv, final Label otherwise)
    // {
    // mv.visitInsn (DUP);
    // mv.visitTypeInsn (INSTANCEOF, "java/lang/Float");
    // mv.visitJumpInsn (IFEQ, otherwise);
    // mv.unbox (Type.FLOAT_TYPE);
    // mv.visitInsn (F2D);
    // }

    /**
     * If there is a Double on the stack, convert it to an double. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param mv Bytecode generator.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2double (final GeneratorAdapter mv, final Label otherwise)
    {
	mv.visitInsn (DUP);
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Double");
	mv.visitJumpInsn (IFEQ, otherwise);
	mv.unbox (Type.DOUBLE_TYPE);
    }

    /**
     * Remove the top stack entry.
     *
     * @param mv Bytecode generator.
     * @param fromClass The class of the top stack entry.
     */
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

    private void convert2char (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
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
	if (allowNarrowing)
	{
	    if (fromClass.equals (int.class) || fromClass.equals (short.class))
	    {
		mv.visitInsn (I2C);
		return;
	    }
	    if (fromClass.equals (Integer.class) || fromClass.equals (Short.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (I2C);
		return;
	    }
	}
	if (fromClass.equals (short.class) || fromClass.equals (int.class) || fromClass.equals (long.class)
	    || fromClass.equals (float.class) || fromClass.equals (double.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class) || fromClass.equals (Long.class) || fromClass.equals (Float.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to char");
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

    private void convert2byte (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
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
	if (fromClass.equals (int.class) || fromClass.equals (short.class))
	{
	    if (allowNarrowing)
	    {
		mv.visitInsn (I2B);
		return;
	    }
	}
	if (fromClass.equals (Integer.class) || fromClass.equals (Short.class))
	{
	    if (allowNarrowing)
	    {
		mv.unbox (Type.INT_TYPE);
		mv.visitInsn (I2B);
		return;
	    }
	}
	// Convert from long to byte in two steps
	// if (fromClass.equals (long.class))
	// {
	// if (allowNarrowing)
	// {
	// mv.visitInsn (L2I);
	// mv.visitInsn (I2B);
	// return;
	// }
	// }
	if (fromClass.equals (int.class) || fromClass.equals (short.class) || fromClass.equals (long.class)
	    || fromClass.equals (float.class) || fromClass.equals (double.class) || fromClass.equals (Integer.class)
	    || fromClass.equals (Short.class) || fromClass.equals (Long.class) || fromClass.equals (Float.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to byte");
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
	if (allowNarrowing)
	{
	    convertInt2int (mv, l3);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l3);
	    convertShort2int (mv, l4);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l4);
	}

	throwException (mv, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to byte");
	mv.visitLabel (l0);
	// Succeed
    }

    private void convert2short (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
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
	if (allowNarrowing)
	{
	    if (fromClass.equals (Integer.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (I2S);
		return;
	    }
	    if (fromClass.equals (int.class))
	    {
		mv.visitInsn (I2S);
		return;
	    }
	    // Convert to int, then to short if required.
	    // if (fromClass.equals (Long.class))
	    // {
	    // mv.unbox (boxer.getUnboxedType (fromType));
	    // mv.visitInsn (L2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	    // if (fromClass.equals (long.class))
	    // {
	    // mv.visitInsn (L2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	    // if (fromClass.equals (Float.class))
	    // {
	    // mv.unbox (boxer.getUnboxedType (fromType));
	    // mv.visitInsn (F2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	    // if (fromClass.equals (float.class))
	    // {
	    // mv.visitInsn (F2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	    // if (fromClass.equals (Double.class))
	    // {
	    // mv.unbox (boxer.getUnboxedType (fromType));
	    // mv.visitInsn (D2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	    // if (fromClass.equals (double.class))
	    // {
	    // mv.visitInsn (D2I);
	    // mv.visitInsn (I2S);
	    // return;
	    // }
	}
	if (fromClass.equals (int.class) || fromClass.equals (long.class) || fromClass.equals (float.class)
	    || fromClass.equals (double.class) || fromClass.equals (Integer.class) || fromClass.equals (Long.class)
	    || fromClass.equals (Float.class) || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to short");
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
	if (allowNarrowing)
	{
	    // (define foo (x) (the short x))
	    convertInt2int (mv, l4);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l4);
	}
	throwException (mv, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to short");
	mv.visitLabel (l0);
    }

    private void convert2int (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
    {
	final Type fromType = Type.getType (fromClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.CHAR || fromSort == Type.BYTE || fromSort == Type.SHORT || fromSort == Type.INT)
	{
	    return; // OK already
	}

	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (boxer.getUnboxedType (fromType));
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (Long.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (L2I);
	    }
	    if (fromClass.equals (long.class))
	    {
		mv.visitInsn (L2I);
	    }
	    if (fromClass.equals (Float.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (F2I);
	    }
	    if (fromClass.equals (float.class))
	    {
		mv.visitInsn (F2I);
	    }
	    if (fromClass.equals (Double.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (D2I);
	    }
	    if (fromClass.equals (double.class))
	    {
		mv.visitInsn (D2I);
	    }
	}
	if (fromClass.equals (long.class) || fromClass.equals (float.class) || fromClass.equals (double.class)
	    || fromClass.equals (Long.class) || fromClass.equals (Float.class) || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to int");
	}

	final Label l0 = new Label (); // Good
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	final Label l6 = new Label ();
	final Label l7 = new Label ();
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
	if (allowNarrowing)
	{
	    // Support long, float and double
	    // (define short:foo (x) short:int:x)
	    // (foo long:4)
	    convertLong2int (mv, l5);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l5);

	    convertFloat2int (mv, l6);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l6);

	    convertDouble2int (mv, l7);
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l7);
	}
	throwException (mv, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to int");
	mv.visitLabel (l0);
	mv.visitLineNumber (888, l0);
    }

    private void convert2long (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	final int fromSort = fromType.getSort ();
	if (fromSort == Type.LONG)
	{
	    return; // OK already
	}
	if (fromSort == Type.CHAR || fromSort == Type.BYTE || fromSort == Type.SHORT || fromSort == Type.INT)
	{
	    mv.visitInsn (I2L); // Extend
	    return;
	}
	if (fromClass.equals (Character.class) || fromClass.equals (Byte.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (I2L); // Extend
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    mv.unbox (toType);
	    return;
	}
	if (fromClass.equals (Float.class) && allowNarrowing)
	{
	    mv.unbox (Type.FLOAT_TYPE);
	    mv.visitInsn (F2L); // Narrow
	    return;
	}
	if (fromClass.equals (float.class) && allowNarrowing)
	{
	    mv.visitInsn (F2L); // Narrow
	    return;
	}
	if (fromClass.equals (Double.class) && allowNarrowing)
	{
	    mv.unbox (Type.DOUBLE_TYPE);
	    mv.visitInsn (D2L); // Narrow
	    return;
	}
	if (fromClass.equals (double.class) && allowNarrowing)
	{
	    mv.visitInsn (D2L); // Narrow
	    return;
	}
	if (fromClass.equals (float.class) || fromClass.equals (double.class) || fromClass.equals (Float.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to long");
	}
	// Last choice: convert and check dynamically
	final Label l0 = new Label (); // got to int
	final Label l00 = new Label (); // got to long
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	final Label l6 = new Label ();
	final Label l7 = new Label ();
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
	if (allowNarrowing)
	{
	    // Support float and double
	    // (define long:foo (x) long:int:x)
	    // (foo double:4)
	    convertFloat2long (mv, l6);
	    mv.visitJumpInsn (GOTO, l00);
	    mv.visitLabel (l6);

	    convertDouble2long (mv, l7);
	    mv.visitJumpInsn (GOTO, l00);
	    mv.visitLabel (l7);
	}
	throwException (mv, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to long");
	mv.visitLabel (l0);
	mv.visitInsn (I2L);
	mv.visitLabel (l00);
    }

    private void convert2float (final GeneratorAdapter mv, final Class<?> fromClass, final Class<?> toClass,
            final boolean allowNarrowing)
    {
	final Type fromType = Type.getType (fromClass);
	// final Type toType = Type.getType (toClass);
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
	if (fromClass.equals (int.class) || fromClass.equals (short.class) || fromClass.equals (byte.class))
	{
	    mv.visitInsn (I2F);
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (Long.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (L2F);
		return;
	    }
	    if (fromClass.equals (long.class))
	    {
		mv.visitInsn (L2F);
		return;
	    }
	    if (fromClass.equals (Double.class))
	    {
		mv.unbox (boxer.getUnboxedType (fromType));
		mv.visitInsn (D2F);
		return;
	    }
	    if (fromClass.equals (double.class))
	    {
		mv.visitInsn (D2F);
		return;
	    }
	}
	if (fromClass.equals (long.class) || fromClass.equals (Long.class) || fromClass.equals (double.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to float");
	}
	// (define float:foo (x) float:x)
	final Label l0 = new Label (); // got to int
	final Label l00 = new Label (); // got to float
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	final Label l3 = new Label ();
	final Label l4 = new Label ();
	final Label l5 = new Label ();
	final Label l6 = new Label ();
	final Label l7 = new Label ();
	convertByte2float (mv, l1);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l1);
	convertCharacter2float (mv, l2);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l2);
	convertShort2float (mv, l3);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l3);
	convertInteger2float (mv, l4);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l4);
	convertFloat2float (mv, l5);
	mv.visitJumpInsn (GOTO, l00);
	mv.visitLabel (l5);
	if (allowNarrowing)
	{
	    // Support long and double
	    convertLong2float (mv, l6);
	    mv.visitJumpInsn (GOTO, l00);
	    mv.visitLabel (l6);

	    convertDouble2float (mv, l7);
	    mv.visitJumpInsn (GOTO, l00);
	    mv.visitLabel (l7);
	}
	throwException (mv, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to float");
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
	    return; // OK already
	}
	if (fromSort == Type.FLOAT)
	{
	    mv.visitInsn (F2D); // Extend
	    return;
	}
	if (fromClass.equals (byte.class) || fromClass.equals (char.class) || fromClass.equals (short.class)
	    || fromClass.equals (int.class))
	{
	    mv.visitInsn (I2D);
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    mv.unbox (boxer.getUnboxedType (fromType));
	    mv.visitInsn (I2D);
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    mv.unbox (toType);
	    mv.visitInsn (L2D);
	    return;
	}
	if (fromClass.equals (long.class))
	{
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
	// The value is an object and we are trying to convert to a specific class.
	// If the value is a primitive, try boxing it
	if (fromType.getSort () != Type.OBJECT && fromType.getSort () != Type.ARRAY)
	{
	    mv.box (fromType);
	}
	// If the value is an Object, try a cast
	mv.visitTypeInsn (CHECKCAST, toType.getInternalName ());
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
