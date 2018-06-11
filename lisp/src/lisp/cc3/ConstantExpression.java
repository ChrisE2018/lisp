
package lisp.cc3;

import java.util.logging.Logger;

import org.objectweb.asm.*;

import lisp.util.LogString;

public class ConstantExpression implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (ConstantExpression.class.getName ());

    public void compileConstantExpression (final MethodVisitor mv, final Object e, final Class<?> valueClass,
            @SuppressWarnings ("unused") final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueClass.equals (boolean.class))
	{
	    if (e instanceof Boolean && !((Boolean)e).booleanValue ())
	    {
		mv.visitInsn (ICONST_0);
		return;
	    }
	    if (liberalTruth)
	    {
		mv.visitInsn (ICONST_1);
		return;
	    }
	    throw new IllegalArgumentException ("Constant " + e + " is not a boolean");
	}
	// Compile constant expressions
	// All of these box the constant in a class wrapper. If we can use the primitive
	// type instead, that is more efficient.
	else if (e instanceof Boolean)
	{
	    mv.visitLdcInsn (e);
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(B)Ljava/lang/Boolean;", false);
	}
	else if (e instanceof Byte)
	{
	    final Byte b = (Byte)e;
	    if (valueClass.equals (byte.class))
	    {
		mv.visitLdcInsn (b.byteValue ());
	    }
	    else if (valueClass.equals (short.class))
	    {
		mv.visitLdcInsn (b.shortValue ());
	    }
	    else if (valueClass.equals (int.class))
	    {
		mv.visitLdcInsn (b.intValue ());
	    }
	    else if (valueClass.equals (long.class))
	    {
		mv.visitLdcInsn (b.longValue ());
	    }
	    else if (valueClass.equals (float.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (b.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (b.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (b);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false);
	    }
	}
	else if (e instanceof Short)
	{
	    final Short s = (Short)e;
	    if (valueClass.equals (short.class))
	    {
		mv.visitLdcInsn (s.shortValue ());
	    }
	    else if (valueClass.equals (int.class))
	    {
		mv.visitLdcInsn (s.intValue ());
	    }
	    else if (valueClass.equals (long.class))
	    {
		mv.visitLdcInsn (s.longValue ());
	    }
	    else if (valueClass.equals (float.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (s.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (s.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (s);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false);
	    }
	}
	else if (e instanceof Integer)
	{
	    final Integer i = (Integer)e;
	    if (valueClass.equals (int.class))
	    {
		mv.visitLdcInsn (i.intValue ());
	    }
	    else if (valueClass.equals (byte.class))
	    {
		mv.visitLdcInsn (i.byteValue ());
	    }
	    else if (valueClass.equals (short.class))
	    {
		mv.visitLdcInsn (i.shortValue ());
	    }
	    else if (valueClass.equals (long.class))
	    {
		mv.visitLdcInsn (i.longValue ());
	    }
	    else if (valueClass.equals (float.class))
	    {
		mv.visitLdcInsn (i.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		mv.visitLdcInsn (i.doubleValue ());
	    }
	    else
	    {
		// This can produce bad results if valueType is not Object.
		mv.visitLdcInsn (i);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	    }
	}
	else if (e instanceof Long)
	{
	    final Long l = (Long)e;
	    if (valueClass.equals (int.class))
	    {
		mv.visitLdcInsn (l.intValue ());
	    }
	    else if (valueClass.equals (byte.class))
	    {
		mv.visitLdcInsn (l.byteValue ());
	    }
	    else if (valueClass.equals (short.class))
	    {
		mv.visitLdcInsn (l.shortValue ());
	    }
	    else if (valueClass.equals (long.class))
	    {
		mv.visitLdcInsn (l.longValue ());
	    }
	    else if (valueClass.equals (float.class))
	    {
		mv.visitLdcInsn (l.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		mv.visitLdcInsn (l.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (l);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false);
	    }
	}
	// CONSIDER Continue the same code pattern...
	else if (e instanceof Float)
	{
	    final Float f = (Float)e;
	    if (valueClass.equals (float.class))
	    {
		mv.visitLdcInsn (f.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		mv.visitLdcInsn (f.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false);
	    }
	}
	else if (e instanceof Double)
	{
	    final Double d = (Double)e;
	    if (valueClass.equals (float.class))
	    {
		mv.visitLdcInsn (d.floatValue ());
	    }
	    else if (valueClass.equals (double.class))
	    {
		mv.visitLdcInsn (d.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
	    }
	}
	else if (e instanceof String)
	{
	    mv.visitLdcInsn (e);
	}
	else
	{
	    LOGGER.info (new LogString ("Ignoring '%s' %s", e, e.getClass ()));
	    mv.visitInsn (ACONST_NULL);
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
