
package lisp.cc4;

import java.util.List;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.cc.Boxer;

/**
 * Class to generate conversion code. There are only two public methods in this class:
 * pushDefaultValue and convert. Added convertIfFalse and convertIfTrue.
 */
public class TreeConverter implements Opcodes
{
    private static final TreeBoxer boxer = new TreeBoxer ();

    public void add (final InsnList il, final AbstractInsnNode insnNode)
    {
	if (insnNode != null)
	{
	    il.add (insnNode);
	}
    }

    /**
     * When a LabelNodeSet is added, we add all the component labels too. A later phase should
     * optimize all but one of these labels out.
     *
     * @param labels
     */
    public void add (final InsnList il, final LabelNodeSet labels)
    {
	il.add (labels);
	for (final LabelNode ln : labels.getLabels ())
	{
	    add (il, ln);
	}
    }

    /**
     * Push a default value of a specified class onto the stack.
     *
     * @param il Instruction list to contain the code.
     * @param valueClass The value type to return.
     * @param booleanDefault If the value will be a primitive boolean, use this as the default
     *            value.
     */
    public void pushDefaultValue (final InsnList il, final Class<?> valueClass, final boolean booleanDefault)
    {
	if (valueClass != null)
	{
	    if (boolean.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode (booleanDefault));
	    }
	    else if (byte.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode ((byte)0));
	    }
	    else if (char.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode ((char)0));
	    }
	    else if (short.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode ((short)0));
	    }
	    else if (int.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode (0));
	    }
	    else if (long.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode (0L));
	    }
	    else if (float.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode (0.0f));
	    }
	    else if (double.class.equals (valueClass))
	    {
		il.add (new LdcInsnNode (0.0));
	    }
	    else
	    {
		il.add (new LdcInsnNode (ACONST_NULL));
	    }
	}
    }

    /**
     * Convert to boolean and jump to label if true and fall through if false. Nothing is left on
     * the stack. (untested)
     */
    public void convertIfTrue (final InsnList il, final CompileResultSet fromClass, final boolean allowNarrowing,
            final boolean liberalTruth, final LabelNodeSet lTrue)
    {
	if (fromClass == null)
	{
	    throw new Error ("Compiler error: can't convert void to boolean");
	}
	final LabelNodeSet lExit = new LabelNodeSet ();
	final List<CompileResult> results = fromClass.getResults ();
	for (int i = 0; i < results.size (); i++)
	{
	    final CompileResult cr = results.get (i);
	    if (cr instanceof ExplicitCompileResult)
	    {
		final ExplicitCompileResult ecr = (ExplicitCompileResult)cr;
		final Class<?> fc = ecr.getResultClass ();
		add (il, cr.getLabel ());
		convert (il, fc, boolean.class, allowNarrowing, liberalTruth);
		il.add (new JumpInsnNode (IFNE, lTrue));
		il.add (new JumpInsnNode (GOTO, lExit));
	    }
	    else if (cr instanceof ImplicitCompileResult)
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)cr;
		final Object value = icr.getValue ();
		if (value instanceof Boolean && (Boolean)value)
		{
		    lTrue.add (cr.getLabel ());
		}
		else
		{
		    lExit.add (cr.getLabel ());
		}
	    }
	}
	add (il, lExit);
    }

    /**
     * Convert to boolean and jump to label if false and fall through if true. Nothing is left on
     * the stack.
     */
    public void convertIfFalse (final InsnList il, final CompileResultSet fromClass, final boolean allowNarrowing,
            final boolean liberalTruth, final LabelNodeSet lFalse)
    {
	if (fromClass == null)
	{
	    throw new Error ("Compiler error: can't convert void to boolean");
	}
	final LabelNodeSet lExit = new LabelNodeSet ();
	final List<CompileResult> results = fromClass.getResults ();
	for (int i = 0; i < results.size (); i++)
	{
	    final CompileResult cr = results.get (i);
	    if (cr instanceof ExplicitCompileResult)
	    {
		final ExplicitCompileResult ecr = (ExplicitCompileResult)cr;
		final Class<?> fc = ecr.getResultClass ();
		add (il, cr.getLabel ());
		convert (il, fc, boolean.class, allowNarrowing, liberalTruth);
		il.add (new JumpInsnNode (IFEQ, lFalse));
		il.add (new JumpInsnNode (GOTO, lExit));
	    }
	    else if (cr instanceof ImplicitCompileResult)
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)cr;
		final Object value = icr.getValue ();
		if (value instanceof Boolean && (Boolean)value)
		{
		    lExit.add (cr.getLabel ());
		}
		else
		{
		    lFalse.add (cr.getLabel ());
		}
	    }
	}
	add (il, lExit);
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
     * @param il Instruction list to contain the code.
     * @param fromType The current type of the top stack element.
     * @param toType The required type of the top stack element. We must convert to this type (or a
     *            subtype).
     * @param allowNarrowing When true, narrowing conversions will be generated if required.
     *            Otherwise narrowing throws and error.
     * @param liberalTruth When set, any non-boolean result is accepted as true. Otherwise, boolean
     *            testing requires strictly boolean values.
     */
    public void convert (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (fromClass == null)
	{
	    convert (il, void.class, toClass, allowNarrowing, liberalTruth);
	    return;
	}
	if (toClass == null)
	{
	    convert (il, fromClass, void.class, allowNarrowing, liberalTruth);
	    return;
	}
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
	    boxer.unbox (il, fromType);
	    return;
	}
	// Check for simple boxing
	if (toType.equals (boxer.getBoxedType (fromType)))
	{
	    boxer.box (il, fromType);
	    return;
	}
	if (fromSort == Type.VOID)
	{
	    pushDefaultValue (il, toClass, false);
	    return;
	}
	switch (toSort)
	{
	    case Type.VOID:
		convert2void (il, fromClass);
		return;
	    case Type.BOOLEAN:
		if (fromSort == Type.OBJECT)
		{
		    if (fromClass.equals (Boolean.class))
		    {
			boxer.unbox (il, fromType);
			return;
		    }
		    else
		    {
			coerceBoolean (il, liberalTruth);
		    }
		    return;
		}
		cantConvert (fromClass, toClass);
		break;

	    case Type.CHAR:
	    {
		convert2char (il, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.BYTE:
	    {
		convert2byte (il, fromClass, toClass, allowNarrowing);

		return;
	    }
	    case Type.SHORT:
	    {
		convert2short (il, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.INT:
	    {
		convert2int (il, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.LONG:
	    {
		convert2long (il, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.FLOAT:
	    {
		convert2float (il, fromClass, toClass, allowNarrowing);
		return;
	    }
	    case Type.DOUBLE:
	    {
		convert2double (il, fromClass, toClass);
		return;
	    }
	    case Type.OBJECT:
	    {
		// [TODO] Special case for String, Symbol?
		convert2Object (il, fromClass, toClass);
		return;
	    }
	    case Type.ARRAY:
	    case Type.METHOD:
	    default:
		cantConvert (fromClass, toClass);
	}
	cantConvert (fromClass, toClass);
    }

    /**
     * Convert value on top of the stack from a Boolean to a boolean. This is used as a last resort
     * when the return type must be boolean and there is no better way to get there. If the top of
     * the stack is not a Boolean, the result left on the stack is always true.
     * <p>
     * This should only be called if liberalTruth is true.
     * </p>
     */
    private void coerceBoolean (final InsnList il, final boolean liberalTruth)
    {
	// (define boolean:foo () true)
	// (define boolean:foo (x) x)
	if (liberalTruth)
	{
	    il.add (new InsnNode (DUP));
	    final LabelNode l1 = new LabelNode ();
	    il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Boolean"));
	    il.add (new JumpInsnNode (IFNE, l1));
	    il.add (new InsnNode (POP));
	    il.add (new LdcInsnNode (true));
	    final LabelNode l2 = new LabelNode ();
	    il.add (new JumpInsnNode (GOTO, l2));
	    il.add (l1);
	    il.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
	    boxer.unbox (il, Type.BOOLEAN_TYPE);
	    il.add (l2);
	}
	else
	{
	    il.add (new InsnNode (DUP));
	    il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Boolean"));
	    final LabelNode l1 = new LabelNode ();
	    il.add (new JumpInsnNode (IFNE, l1));
	    il.add (new InsnNode (POP));
	    throwException (il, "java/lang/IllegalArgumentException", "Boolean required");
	    il.add (l1);
	    il.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
	    boxer.unbox (il, Type.BOOLEAN_TYPE);
	}
    }

    // private void throwException (final InsnList il, final String internalName)
    // {
    // il.add (new TypeInsnNode (NEW, internalName));
    // il.add (new InsnNode (DUP));
    // il.add (new MethodInsnNode (INVOKESPECIAL, internalName, "<init>", "()V", false));
    // il.add (new InsnNode (ATHROW));
    // }

    private void throwException (final InsnList il, final String internalName, final String format, final Object... args)
    {
	il.add (new TypeInsnNode (NEW, internalName));
	il.add (new InsnNode (DUP));
	final String message = String.format (format, args);
	il.add (new LdcInsnNode (message));
	il.add (new MethodInsnNode (INVOKESPECIAL, internalName, "<init>", "(Ljava/lang/String;)V", false));
	il.add (new InsnNode (ATHROW));
    }

    private void cantConvert (final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	throw new Error ("Can't convert from " + fromClass + "(" + fromType + ") to " + toClass + "(" + toType + ")");
    }

    /**
     * If there is a Byte on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Byte.
     */
    private void convertByte2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Byte"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Byte"));
	boxer.unbox (il, Type.BYTE_TYPE);
    }

    /**
     * If there is a Character on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Character.
     */
    private void convertChar2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Character"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Character"));
	boxer.unbox (il, Type.CHAR_TYPE);
    }

    /**
     * If there is a Short on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Short.
     */
    private void convertShort2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Short"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Short"));
	boxer.unbox (il, Type.SHORT_TYPE);
    }

    /**
     * If there is a Integer on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Integer.
     */
    private void convertInt2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Integer"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Integer"));
	boxer.unbox (il, Type.INT_TYPE);
    }

    /**
     * If there is a Long on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Long"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Long"));
	boxer.unbox (il, Type.LONG_TYPE);
	il.add (new InsnNode (L2I));
    }

    /**
     * If there is a Float on the stack, convert it to an int. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Float"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Float"));
	boxer.unbox (il, Type.FLOAT_TYPE);
	il.add (new InsnNode (F2I));
    }

    /**
     * If there is a Double on the stack, convert it to an int. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2int (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Double"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Double"));
	boxer.unbox (il, Type.DOUBLE_TYPE);
	il.add (new InsnNode (D2I));
    }

    /**
     * If there is a Long on the stack, convert it to an long. Otherwise, leave the top value on the
     * stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2long (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Long"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Long"));
	boxer.unbox (il, Type.LONG_TYPE);
    }

    /**
     * If there is a Float on the stack, convert it to an long. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2long (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Float"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Float"));
	boxer.unbox (il, Type.FLOAT_TYPE);
	il.add (new InsnNode (F2L));
    }

    /**
     * If there is a Double on the stack, convert it to an long. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2long (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Double"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Double"));
	boxer.unbox (il, Type.DOUBLE_TYPE);
	il.add (new InsnNode (D2L));
    }

    /**
     * If there is a Byte on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Byte.
     */
    private void convertByte2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Byte"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Byte"));
	boxer.unbox (il, Type.BYTE_TYPE);
	il.add (new InsnNode (I2F));
    }

    /**
     * If there is a Character on the stack, convert it to an float. Otherwise, leave the top value
     * on the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Character.
     */
    private void convertCharacter2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Character"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Character"));
	boxer.unbox (il, Type.CHAR_TYPE);
	il.add (new InsnNode (I2F));
    }

    /**
     * If there is a Short on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Short.
     */
    private void convertShort2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Short"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Short"));
	boxer.unbox (il, Type.SHORT_TYPE);
	il.add (new InsnNode (I2F));
    }

    /**
     * If there is a Integer on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Integer.
     */
    private void convertInteger2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Integer"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Integer"));
	boxer.unbox (il, Type.INT_TYPE);
	il.add (new InsnNode (I2F));
    }

    /**
     * If there is a Float on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Float.
     */
    private void convertFloat2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Float"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Float"));
	boxer.unbox (il, Type.FLOAT_TYPE);
    }

    /**
     * If there is a Long on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Long.
     */
    private void convertLong2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Long"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Long"));
	boxer.unbox (il, Type.LONG_TYPE);
	il.add (new InsnNode (L2F));
    }

    /**
     * If there is a Double on the stack, convert it to an float. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2float (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Double"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Double"));
	boxer.unbox (il, Type.DOUBLE_TYPE);
	il.add (new InsnNode (D2F));
    }

    // /**
    // * If there is a Float on the stack, convert it to a double. Otherwise, leave the top value on
    // * the stack alone and jump to the label otherwise.
    // *
    // * @param il Instruction list to contain the code.
    // * @param otherwise Label to jump to if the stack does not have a Float.
    // */
    // private void convertFloat2double (final InsnList il, final LabelNode otherwise)
    // {
    // il.add (new InsnNode (DUP));
    // il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Float"));
    // il.add (new JumpInsnNode (IFEQ, otherwise));
    // il.add (new TypeInsnNode (CHECKCAST, "java/lang/Float"));
    // boxer.unbox (il, Type.FLOAT_TYPE);
    // il.add (new InsnNode (F2D));
    // }

    /**
     * If there is a Double on the stack, convert it to an double. Otherwise, leave the top value on
     * the stack alone and jump to the label otherwise.
     *
     * @param il Instruction list to contain the code.
     * @param otherwise Label to jump to if the stack does not have a Double.
     */
    private void convertDouble2double (final InsnList il, final LabelNode otherwise)
    {
	il.add (new InsnNode (DUP));
	il.add (new TypeInsnNode (INSTANCEOF, "java/lang/Double"));
	il.add (new JumpInsnNode (IFEQ, otherwise));
	il.add (new TypeInsnNode (CHECKCAST, "java/lang/Double"));
	boxer.unbox (il, Type.DOUBLE_TYPE);
    }

    /**
     * Remove the top stack entry.
     *
     * @param il Instruction list to contain the code.
     * @param fromClass The class of the top stack entry.
     */
    private void convert2void (final InsnList il, final Class<?> fromClass)
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
	    il.add (new InsnNode (POP));
	}
	else if (fromSize == 2)
	{
	    il.add (new InsnNode (POP2));
	}
    }

    private void convert2char (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    boxer.unbox (il, toType);
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (int.class) || fromClass.equals (short.class))
	    {
		il.add (new InsnNode (I2C));
		return;
	    }
	    if (fromClass.equals (Integer.class) || fromClass.equals (Short.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (I2C));
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
	final LabelNode l0 = new LabelNode (); // Good
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l2);
	throwException (il, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);
	il.add (l0);
	// Succeed
    }

    private void convert2byte (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    boxer.unbox (il, toType);
	    return;
	}
	if (fromClass.equals (int.class) || fromClass.equals (short.class))
	{
	    if (allowNarrowing)
	    {
		il.add (new InsnNode (I2B));
		return;
	    }
	}
	if (fromClass.equals (Integer.class) || fromClass.equals (Short.class))
	{
	    if (allowNarrowing)
	    {
		boxer.unbox (il, Type.INT_TYPE);
		il.add (new InsnNode (I2B));
		return;
	    }
	}
	// Convert from long to byte in two steps
	// if (fromClass.equals (long.class))
	// {
	// if (allowNarrowing)
	// {
	// il.add (new InsnNode (L2I));
	// il.add(new Insn (I2B);
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
	final LabelNode l0 = new LabelNode (); // Good
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l2);
	if (allowNarrowing)
	{
	    convertInt2int (il, l3);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l3);
	    convertShort2int (il, l4);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l4);
	}

	throwException (il, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to byte");
	il.add (l0);
	// Succeed
    }

    private void convert2short (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    boxer.unbox (il, boxer.getUnboxedType (fromType));
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (Integer.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (I2S));
		return;
	    }
	    if (fromClass.equals (int.class))
	    {
		il.add (new InsnNode (I2S));
		return;
	    }
	    // Convert to int, then to short if required.
	    // if (fromClass.equals (Long.class))
	    // {
	    // boxer.unbox (il,boxer.getUnboxedType (fromType));
	    // il.add (new InsnNode (L2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	    // if (fromClass.equals (long.class))
	    // {
	    // il.add (new InsnNode (L2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	    // if (fromClass.equals (Float.class))
	    // {
	    // boxer.unbox (il,boxer.getUnboxedType (fromType));
	    // il.add (new InsnNode (F2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	    // if (fromClass.equals (float.class))
	    // {
	    // il.add (new InsnNode (F2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	    // if (fromClass.equals (Double.class))
	    // {
	    // boxer.unbox (il,boxer.getUnboxedType (fromType));
	    // il.add (new InsnNode (D2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	    // if (fromClass.equals (double.class))
	    // {
	    // il.add (new InsnNode (D2I));
	    // il.add (new InsnNode (I2S));
	    // return;
	    // }
	}
	if (fromClass.equals (int.class) || fromClass.equals (long.class) || fromClass.equals (float.class)
	    || fromClass.equals (double.class) || fromClass.equals (Integer.class) || fromClass.equals (Long.class)
	    || fromClass.equals (Float.class) || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to short");
	}
	final LabelNode l0 = new LabelNode (); // Good
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l2);
	convertShort2int (il, l3);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l3);
	if (allowNarrowing)
	{
	    // (define foo (x) (the short x))
	    convertInt2int (il, l4);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l4);
	}
	throwException (il, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to short");
	il.add (l0);
    }

    private void convert2int (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    boxer.unbox (il, boxer.getUnboxedType (fromType));
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (Long.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (L2I));
	    }
	    if (fromClass.equals (long.class))
	    {
		il.add (new InsnNode (L2I));
	    }
	    if (fromClass.equals (Float.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (F2I));
	    }
	    if (fromClass.equals (float.class))
	    {
		il.add (new InsnNode (F2I));
	    }
	    if (fromClass.equals (Double.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (D2I));
	    }
	    if (fromClass.equals (double.class))
	    {
		il.add (new InsnNode (D2I));
	    }
	}
	if (fromClass.equals (long.class) || fromClass.equals (float.class) || fromClass.equals (double.class)
	    || fromClass.equals (Long.class) || fromClass.equals (Float.class) || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to int");
	}

	final LabelNode l0 = new LabelNode (); // Good
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	final LabelNode l5 = new LabelNode ();
	final LabelNode l6 = new LabelNode ();
	final LabelNode l7 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l2);
	convertShort2int (il, l3);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l3);
	convertInt2int (il, l4);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l4);
	if (allowNarrowing)
	{
	    // Support long, float and double
	    // (define short:foo (x) short:int:x)
	    // (foo long:4)
	    convertLong2int (il, l5);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l5);

	    convertFloat2int (il, l6);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l6);

	    convertDouble2int (il, l7);
	    il.add (new JumpInsnNode (GOTO, l0));
	    il.add (l7);
	}
	throwException (il, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to int");
	il.add (l0);
    }

    private void convert2long (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    il.add (new InsnNode (I2L)); // Extend
	    return;
	}
	if (fromClass.equals (Character.class) || fromClass.equals (Byte.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    boxer.unbox (il, toType);
	    il.add (new InsnNode (I2L)); // Extend
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    boxer.unbox (il, toType);
	    return;
	}
	if (fromClass.equals (Float.class) && allowNarrowing)
	{
	    boxer.unbox (il, Type.FLOAT_TYPE);
	    il.add (new InsnNode (F2L)); // Narrow
	    return;
	}
	if (fromClass.equals (float.class) && allowNarrowing)
	{
	    il.add (new InsnNode (F2L)); // Narrow
	    return;
	}
	if (fromClass.equals (Double.class) && allowNarrowing)
	{
	    boxer.unbox (il, Type.DOUBLE_TYPE);
	    il.add (new InsnNode (D2L)); // Narrow
	    return;
	}
	if (fromClass.equals (double.class) && allowNarrowing)
	{
	    il.add (new InsnNode (D2L)); // Narrow
	    return;
	}
	if (fromClass.equals (float.class) || fromClass.equals (double.class) || fromClass.equals (Float.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to long");
	}
	// Last choice: convert and check dynamically
	final LabelNode l0 = new LabelNode (); // got to int
	final LabelNode l00 = new LabelNode (); // got to long
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	final LabelNode l5 = new LabelNode ();
	final LabelNode l6 = new LabelNode ();
	final LabelNode l7 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l2);
	convertShort2int (il, l3);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l3);
	convertInt2int (il, l4);
	il.add (new JumpInsnNode (GOTO, l0));
	il.add (l4);
	convertLong2long (il, l5);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l5);
	if (allowNarrowing)
	{
	    // Support float and double
	    // (define long:foo (x) long:int:x)
	    // (foo double:4)
	    convertFloat2long (il, l6);
	    il.add (new JumpInsnNode (GOTO, l00));
	    il.add (l6);

	    convertDouble2long (il, l7);
	    il.add (new JumpInsnNode (GOTO, l00));
	    il.add (l7);
	}
	throwException (il, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to long");
	il.add (l0);
	il.add (new InsnNode (I2L));
	il.add (l00);
    }

    private void convert2float (final InsnList il, final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing)
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
	    boxer.unbox (il, boxer.getUnboxedType (fromType));
	    return;
	}
	if (fromClass.equals (int.class) || fromClass.equals (short.class) || fromClass.equals (byte.class))
	{
	    il.add (new InsnNode (I2F));
	    return;
	}
	if (allowNarrowing)
	{
	    if (fromClass.equals (Long.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (L2F));
		return;
	    }
	    if (fromClass.equals (long.class))
	    {
		il.add (new InsnNode (L2F));
		return;
	    }
	    if (fromClass.equals (Double.class))
	    {
		boxer.unbox (il, boxer.getUnboxedType (fromType));
		il.add (new InsnNode (D2F));
		return;
	    }
	    if (fromClass.equals (double.class))
	    {
		il.add (new InsnNode (D2F));
		return;
	    }
	}
	if (fromClass.equals (long.class) || fromClass.equals (Long.class) || fromClass.equals (double.class)
	    || fromClass.equals (Double.class))
	{
	    throw new IllegalArgumentException ("Use 'the' for explicit narrowing conversion to float");
	}
	// (define float:foo (x) float:x)
	final LabelNode l0 = new LabelNode (); // got to int
	final LabelNode l00 = new LabelNode (); // got to float
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	final LabelNode l5 = new LabelNode ();
	final LabelNode l6 = new LabelNode ();
	final LabelNode l7 = new LabelNode ();
	convertByte2float (il, l1);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l1);
	convertCharacter2float (il, l2);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l2);
	convertShort2float (il, l3);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l3);
	convertInteger2float (il, l4);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l4);
	convertFloat2float (il, l5);
	il.add (new JumpInsnNode (GOTO, l00));
	il.add (l5);
	if (allowNarrowing)
	{
	    // Support long and double
	    convertLong2float (il, l6);
	    il.add (new JumpInsnNode (GOTO, l00));
	    il.add (l6);

	    convertDouble2float (il, l7);
	    il.add (new JumpInsnNode (GOTO, l00));
	    il.add (l7);
	}
	throwException (il, "java/lang/IllegalArgumentException", "Use 'the' for explicit narrowing conversion to float");
	il.add (l0);
	il.add (new InsnNode (I2F));
	il.add (l00);
    }

    private void convert2double (final InsnList il, final Class<?> fromClass, final Class<?> toClass)
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
	    il.add (new InsnNode (F2D)); // Extend
	    return;
	}
	if (fromClass.equals (byte.class) || fromClass.equals (char.class) || fromClass.equals (short.class)
	    || fromClass.equals (int.class))
	{
	    il.add (new InsnNode (I2D));
	    return;
	}
	if (fromClass.equals (Byte.class) || fromClass.equals (Character.class) || fromClass.equals (Short.class)
	    || fromClass.equals (Integer.class))
	{
	    boxer.unbox (il, boxer.getUnboxedType (fromType));
	    il.add (new InsnNode (I2D));
	    return;
	}
	if (fromClass.equals (Long.class))
	{
	    boxer.unbox (il, toType);
	    il.add (new InsnNode (L2D));
	    return;
	}
	if (fromClass.equals (long.class))
	{
	    il.add (new InsnNode (L2D));
	    return;
	}
	if (fromClass.equals (Float.class))
	{
	    boxer.unbox (il, toType);
	    il.add (new InsnNode (F2D));
	    return;
	}
	if (fromClass.equals (Double.class))
	{
	    boxer.unbox (il, toType);
	    return;
	}
	// Last choice: convert and check dynamically
	final LabelNode lint = new LabelNode (); // got to int
	final LabelNode llong = new LabelNode (); // got to long
	final LabelNode lfloat = new LabelNode (); // got to double
	final LabelNode ldouble = new LabelNode (); // got to double
	final LabelNode l1 = new LabelNode ();
	final LabelNode l2 = new LabelNode ();
	final LabelNode l3 = new LabelNode ();
	final LabelNode l4 = new LabelNode ();
	final LabelNode l5 = new LabelNode ();
	final LabelNode l6 = new LabelNode ();
	final LabelNode l7 = new LabelNode ();
	convertByte2int (il, l1);
	il.add (new JumpInsnNode (GOTO, lint));
	il.add (l1);
	convertChar2int (il, l2);
	il.add (new JumpInsnNode (GOTO, lint));
	il.add (l2);
	convertShort2int (il, l3);
	il.add (new JumpInsnNode (GOTO, lint));
	il.add (l3);
	convertInt2int (il, l4);
	il.add (new JumpInsnNode (GOTO, lint));
	il.add (l4);
	convertLong2long (il, l5);
	il.add (new JumpInsnNode (GOTO, llong));
	il.add (l5);

	convertFloat2float (il, l6);
	il.add (new JumpInsnNode (GOTO, lfloat));
	il.add (l6);

	convertDouble2double (il, l7);
	il.add (new JumpInsnNode (GOTO, ldouble));
	il.add (l7);

	throwException (il, "java/lang/IllegalArgumentException", "Can't convert to %s", toClass);

	il.add (lint);
	il.add (new InsnNode (I2D));
	il.add (new JumpInsnNode (GOTO, ldouble));

	il.add (llong);
	il.add (new InsnNode (L2D));
	il.add (new JumpInsnNode (GOTO, ldouble));

	il.add (lfloat);
	il.add (new InsnNode (F2D));
	il.add (ldouble);
    }

    private void convert2Object (final InsnList il, final Class<?> fromClass, final Class<?> toClass)
    {
	final Type fromType = Type.getType (fromClass);
	final Type toType = Type.getType (toClass);
	// Need to check that the classes are compatible
	if (toClass.isAssignableFrom (fromClass))
	{
	    il.add (new TypeInsnNode (CHECKCAST, toType.getInternalName ()));
	    return;
	}
	final Type boxedFromType = boxer.getBoxedType (fromType);
	if (toType.equals (boxedFromType))
	{
	    boxer.box (il, fromType);
	    return;
	}
	if (toType.equals (Boxer.OBJECT_TYPE))
	{
	    boxer.box (il, fromType);
	    il.add (new TypeInsnNode (CHECKCAST, toType.getInternalName ()));
	    return;
	}
	// The value is an object and we are trying to convert to a specific class.
	// If the value is a primitive, try boxing it
	if (fromType.getSort () != Type.OBJECT && fromType.getSort () != Type.ARRAY)
	{
	    boxer.box (il, fromType);
	}
	// If the value is an Object, try a cast
	il.add (new TypeInsnNode (CHECKCAST, toType.getInternalName ()));
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
