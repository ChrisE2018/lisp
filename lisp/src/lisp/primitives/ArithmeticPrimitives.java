
package lisp.primitives;

import lisp.eval.*;

/**
 * Implementation of basic arithmetic operators. Definitions of plus, minus, times, quotient, =,
 * <, >=, <=, >=, trig, abs, sign
 *
 * @author cre
 */
public class ArithmeticPrimitives extends Definer
{
    // CONSIDER Inverse trig (asin, acos, atan)
    // CONSIDER Exponentials, log
    // CONSIDER Consider using bigdecimal
    // CONSIDER Random
    // CONSIDER Statistical
    // CONSIDER Case for 'long' needed everywhere

    @DefineLisp (classname = "lisp.special.NotFunction")
    public boolean not (final Object arg)
    {
	if (arg instanceof Boolean)
	{
	    final boolean b = (Boolean)arg;
	    return !b;
	}
	return false;
    }

    /**
     * Boolean not. Note that the compiler support class works both here and in the general case.
     */
    @DefineLisp (classname = "lisp.special.NotFunction")
    public boolean not (final boolean arg)
    {
	return !arg;
    }

    @DefineLisp (name = "null", classname = "lisp.special.NullFunction")
    public boolean nullPredicate (final Object arg)
    {
	return arg == null;
    }

    /** Zero test for integers. We don't define zero test for floating point numbers. */
    @DefineLisp (classname = "lisp.special.ZeropIntFunction")
    public boolean zerop (final int a)
    {
	return a == 0;
    }

    /** Zero test for long integers. We don't define zero test for floating point numbers. */
    @DefineLisp (classname = "lisp.special.ZeropLongFunction")
    public boolean zerop (final long a)
    {
	return a == 0;
    }

    /** Remainder for integers. We don't define remainder for floating point numbers. */
    @DefineLisp (classname = "lisp.special.RemIntFunction")
    public int rem (final int a, final int b)
    {
	return a % b;
    }

    /** Remainder for long integers. We don't define remainder for floating point numbers. */
    @DefineLisp (classname = "lisp.special.RemLongFunction")
    public long rem (final long a, final long b)
    {
	return a % b;
    }

    /**
     * One plus for integers. Direct calls to this method can be made if the compiler knows the
     * argument type.
     */
    @DefineLisp (name = "1+", classname = "lisp.special.PlusOneIntFunction")
    public int addOne (final int x)
    {
	return x + 1;
    }

    /**
     * One plus for double. Direct calls to this method can be made if the compiler knows the
     * argument type.
     */
    @DefineLisp (name = "1+", classname = "lisp.special.PlusOneDoubleFunction")
    public double addOne (final double x)
    {
	return x + 1;
    }

    @DefineLisp (name = "1+"// , classname = "lisp.special.PlusOneFunction"
    )
    public Number addOne (final Number x)
    {
	if (x instanceof Integer)
	{
	    return (Integer)x + 1;
	}
	else if (x instanceof Double)
	{
	    return (Double)x + 1;
	}
	else if (x instanceof Short)
	{
	    return (Short)x + 1;
	}
	else if (x instanceof Byte)
	{
	    return (Byte)x + 1;
	}
	else if (x instanceof Float)
	{
	    return (Float)x + 1;
	}
	else
	{
	    return (double)x + 1;
	}
    }

    @DefineLisp (name = "1-", classname = "lisp.special.SubOneIntFunction")
    public int subOne (final int x)
    {
	return x - 1;
    }

    @DefineLisp (name = "1-", classname = "lisp.special.SubOneDoubleFunction")
    public double subOne (final double x)
    {
	return x - 1;
    }

    @DefineLisp (name = "1-")
    public Number subOne (final Number x)
    {
	if (x instanceof Integer)
	{
	    return (Integer)x - 1;
	}
	else if (x instanceof Double)
	{
	    return (Double)x - 1;
	}
	else if (x instanceof Short)
	{
	    return (Short)x - 1;
	}
	else if (x instanceof Byte)
	{
	    return (Byte)x - 1;
	}
	else if (x instanceof Float)
	{
	    return (Float)x - 1;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + x);
	}
    }

    @DefineLisp (name = "+", classname = "lisp.special.PlusIntFunction")
    public int plus (final int a, final int b)
    {
	return a + b;
    }

    @DefineLisp (name = "+", classname = "lisp.special.PlusLongFunction")
    public long plus (final long a, final long b)
    {
	return a + b;
    }

    @DefineLisp (name = "+", classname = "lisp.special.PlusDoubleFunction")
    public double plus (final double a, final double b)
    {
	return a + b;
    }

    @DefineLisp (name = "+")
    public Object plus (final Object... arguments)
    {
	if (arguments.length > 0)
	{
	    Object result = arguments[0];
	    for (int i = 1; i < arguments.length; i++)
	    {
		result = plus (result, arguments[i]);
	    }
	    return result;
	}
	return 0;
    }

    private Object plus (final Integer a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a + (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a + (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a + (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a + (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a + (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object plus (final Double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a + (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a + (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a + (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a + (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a + (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    @DefineLisp
    public Object plus (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return plus ((Integer)a, b);
	}
	else if (a instanceof Double)
	{
	    return plus ((Double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    final Integer aaa = aa;
	    return plus (aaa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    final Integer aaa = aa;
	    return plus (aaa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    final Double aaa = aa;
	    return plus (aaa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp (name = "-", classname = "lisp.special.MinusIntFunction")
    public int minus (final int a, final int b)
    {
	return a - b;
    }

    @DefineLisp (name = "-", classname = "lisp.special.MinusLongFunction")
    public long minus (final long a, final long b)
    {
	return a - b;
    }

    @DefineLisp (name = "-", classname = "lisp.special.MinusDoubleFunction")
    public double minus (final double a, final double b)
    {
	return a - b;
    }

    @DefineLisp (name = "-")
    public Object minus (final Object from, final Object... arguments)
    {
	if (arguments.length == 0)
	{
	    return minus (0, from);
	}
	else
	{
	    Object result = from;
	    for (int i = 0; i < arguments.length; i++)
	    {
		result = minus (result, arguments[i]);
	    }
	    return result;
	}
    }

    private Object minus (final Integer a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a - (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a - (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a - (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a - (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a - (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object minus (final Double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a - (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a - (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a - (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a - (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a - (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object minus (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return minus ((Integer)a, b);
	}
	else if (a instanceof Double)
	{
	    return minus ((Double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    final Integer aaa = aa;
	    return minus (aaa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    final Integer aaa = aa;
	    return minus (aaa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    final Double aaa = aa;
	    return minus (aaa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp (name = "*", classname = "lisp.special.TimesIntFunction")
    public int times (final int a, final int b)
    {
	return a * b;
    }

    @DefineLisp (name = "*", classname = "lisp.special.TimesLongFunction")
    public long times (final long a, final long b)
    {
	return a * b;
    }

    @DefineLisp (name = "*", classname = "lisp.special.TimesDoubleFunction")
    public double times (final double a, final double b)
    {
	return a * b;
    }

    @DefineLisp (name = "*")
    public Object times (final Object... arguments)
    {
	if (arguments.length > 0)
	{
	    Object result = arguments[0];
	    for (int i = 1; i < arguments.length; i++)
	    {
		result = times (result, arguments[i]);
	    }
	    return result;
	}
	return 1;
    }

    private Object times (final Integer a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a * (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a * (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a * (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a * (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a * (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object times (final Double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a * (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a * (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a * (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a * (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a * (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object times (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return times ((Integer)a, b);
	}
	else if (a instanceof Double)
	{
	    return times ((Double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    final Integer aaa = aa;
	    return times (aaa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    final Integer aaa = aa;
	    return times (aaa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    final Double aaa = aa;
	    return times (aaa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp (name = "/", classname = "lisp.special.QuotientIntFunction")
    public int quotient (final int a, final int b)
    {
	return a / b;
    }

    @DefineLisp (name = "/", classname = "lisp.special.QuotientLongFunction")
    public long quotient (final long a, final long b)
    {
	return a / b;
    }

    @DefineLisp (name = "/", classname = "lisp.special.QuotientDoubleFunction")
    public double quotient (final double a, final double b)
    {
	return a / b;
    }

    @DefineLisp (name = "/")
    public Object quotient (final Object from, final Object... arguments)
    {
	if (arguments.length == 0)
	{
	    return quotient (1.0, from);
	}
	else
	{
	    final Object numerator = from;
	    Object denominator = arguments[0];
	    for (int i = 1; i < arguments.length; i++)
	    {
		denominator = times (denominator, arguments[i]);
	    }
	    return quotient (numerator, denominator);
	}
    }

    private Object quotient (final Integer a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a / (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a / (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a / (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a - (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a / (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object quotient (final Double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a / (Integer)b;
	}
	else if (b instanceof Double)
	{
	    return a / (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a / (Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a / (Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a / (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private Object quotient (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return quotient ((Integer)a, b);
	}
	else if (a instanceof Double)
	{
	    return quotient ((Double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    final Integer aaa = aa;
	    return quotient (aaa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    final Integer aaa = aa;
	    return quotient (aaa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    final Double aaa = aa;
	    return quotient (aaa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    // Generic comparison

    /** Compare all arguments with the Java equals method. */
    @DefineLisp
    public boolean equals (final Object a, final Object... arguments)
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    if (!a.equals (arguments[i]))
	    {
		return false;
	    }
	}
	return true;
    }

    @DefineLisp
    public boolean eq (final Object a, final Object... arguments)
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    if (a != arguments[i])
	    {
		return false;
	    }
	}
	return true;
    }

    // Arithmetic comparison

    @DefineLisp (name = "<")
    public boolean lessp (final Object a, final Object... arguments)
    {
	Object previous = a;
	for (int i = 0; i < arguments.length; i++)
	{
	    if (!lessp (previous, arguments[i]))
	    {
		return false;
	    }
	    previous = arguments[i];
	}
	return true;
    }

    @DefineLisp (name = ">")
    public boolean greaterp (final Object a, final Object... arguments)
    {
	Object previous = a;
	for (int i = 0; i < arguments.length; i++)
	{
	    if (!lessp (arguments[i], previous))
	    {
		return false;
	    }
	    previous = arguments[i];
	}
	return true;
    }

    @DefineLisp (name = "<=", classname = "lisp.special.CompareLEIntFunction")
    public boolean compareLE (final int a, final int b)
    {
	return a <= b;
    }

    @DefineLisp (name = "<=")
    public boolean compareLE (final Object a, final Object... arguments)
    {
	Object previous = a;
	for (int i = 0; i < arguments.length; i++)
	{
	    // Invert operator and reverse arguments
	    if (lessp (arguments[i], previous))
	    {
		return false;
	    }
	    previous = arguments[i];
	}
	return true;
    }

    @DefineLisp (name = ">=")
    public boolean compareGE (final Object a, final Object... arguments)
    {
	Object previous = a;
	for (int i = 0; i < arguments.length; i++)
	{
	    // Invert operator and reverse arguments
	    if (lessp (previous, arguments[i]))
	    {
		return false;
	    }
	    previous = arguments[i];
	}
	return true;
    }

    private boolean lessp (final Object a, final Object b)
    {
	if (a instanceof Integer)
	{
	    return lessp ((int)a, b);
	}
	else if (a instanceof Long)
	{
	    return lessp ((long)a, b);
	}
	else if (a instanceof Double)
	{
	    return lessp ((double)a, b);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    return lessp (aa, b);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    return lessp (aa, b);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    return lessp (aa, b);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    private boolean lessp (final long a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a < (int)b;
	}
	else if (b instanceof Long)
	{
	    return a < (long)b;
	}
	else if (b instanceof Double)
	{
	    return (double)a < (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a < (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a < (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return (float)a < (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private boolean lessp (final int a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a < (int)b;
	}
	else if (b instanceof Long)
	{
	    return a < (long)b;
	}
	else if (b instanceof Double)
	{
	    return (double)a < (Double)b;
	}
	else if (b instanceof Short)
	{
	    return a < (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a < (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return (float)a < (Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    private boolean lessp (final double a, final Object b)
    {
	if (b instanceof Integer)
	{
	    return a < (Integer)b;
	}
	else if (b instanceof Long)
	{
	    return a < (Long)b;
	}
	else if (b instanceof Double)
	{
	    return a < (double)b;
	}
	else if (b instanceof Short)
	{
	    return a < (int)(Short)b;
	}
	else if (b instanceof Byte)
	{
	    return a < (int)(Byte)b;
	}
	else if (b instanceof Float)
	{
	    return a < (double)(Float)b;
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + b);
	}
    }

    // Trig, abs sign
    @DefineLisp (name = "abs")
    public Object abs (final Object a)
    {
	if (a instanceof Integer)
	{
	    return Math.abs ((int)a);
	}
	else if (a instanceof Double)
	{
	    return Math.abs ((double)a);
	}
	else if (a instanceof Short)
	{
	    final short aa = (Short)a;
	    return Math.abs (aa);
	}
	else if (a instanceof Byte)
	{
	    final byte aa = (Byte)a;
	    return Math.abs (aa);
	}
	else if (a instanceof Float)
	{
	    final float aa = (Float)a;
	    return Math.abs (aa);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp (name = "sign")
    public int sign (final Object a)
    {
	if (a instanceof Integer)
	{
	    return sign ((int)a);
	}
	else if (a instanceof Double)
	{
	    return sign ((double)a);
	}
	else if (a instanceof Short)
	{
	    final int aa = (Short)a;
	    return sign (aa);
	}
	else if (a instanceof Byte)
	{
	    final int aa = (Byte)a;
	    return sign (aa);
	}
	else if (a instanceof Float)
	{
	    final double aa = (Float)a;
	    return sign (aa);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    private int sign (final int x)
    {
	if (x > 0)
	{
	    return 1;
	}
	else if (x < 0)
	{
	    return -1;
	}
	else
	{
	    return 0;
	}
    }

    private int sign (final double x)
    {
	if (x > 0)
	{
	    return 1;
	}
	else if (x < 0)
	{
	    return -1;
	}
	else
	{
	    return 0;
	}
    }

    @DefineLisp
    public double sin (final Object a)
    {
	if (a instanceof Integer)
	{
	    return Math.sin ((int)a);
	}
	else if (a instanceof Double)
	{
	    return Math.sin ((double)a);
	}
	else if (a instanceof Short)
	{
	    final short aa = (Short)a;
	    return Math.sin (aa);
	}
	else if (a instanceof Byte)
	{
	    final byte aa = (Byte)a;
	    return Math.sin (aa);
	}
	else if (a instanceof Float)
	{
	    final float aa = (Float)a;
	    return Math.sin (aa);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp
    public double cos (final Object a)
    {
	if (a instanceof Integer)
	{
	    return Math.cos ((int)a);
	}
	else if (a instanceof Double)
	{
	    return Math.cos ((double)a);
	}
	else if (a instanceof Short)
	{
	    final short aa = (Short)a;
	    return Math.cos (aa);
	}
	else if (a instanceof Byte)
	{
	    final byte aa = (Byte)a;
	    return Math.cos (aa);
	}
	else if (a instanceof Float)
	{
	    final float aa = (Float)a;
	    return Math.cos (aa);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }

    @DefineLisp
    public double tan (final Object a)
    {
	if (a instanceof Integer)
	{
	    return Math.tan ((int)a);
	}
	else if (a instanceof Double)
	{
	    return Math.tan ((double)a);
	}
	else if (a instanceof Short)
	{
	    final short aa = (Short)a;
	    return Math.tan (aa);
	}
	else if (a instanceof Byte)
	{
	    final byte aa = (Byte)a;
	    return Math.tan (aa);
	}
	else if (a instanceof Float)
	{
	    final float aa = (Float)a;
	    return Math.tan (aa);
	}
	else
	{
	    throw new IllegalArgumentException ("Number required " + a);
	}
    }
}
