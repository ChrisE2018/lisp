
package lisp.special;

import java.util.*;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.asm.instructions.LabelNode;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class TheFunction implements LispTreeFunction, LispCCFunction, Opcodes, LispTreeWalker
{
    private static Object[][] TYPE_TO_CLASS =
        {
         {"byte", byte.class},
         {"char", char.class},
         {"short", short.class},
         {"int", int.class},
         {"long", long.class},
         {"float", float.class},
         {"double", double.class},
         {"Byte", Byte.class},
         {"Char", Character.class},
         {"Short", Short.class},
         {"Int", Integer.class},
         {"Long", Long.class},
         {"Float", Float.class},
         {"Double", Double.class},
         {"Object", Object.class}};

    private final Map<String, Class<?>> typeToClass = new HashMap<String, Class<?>> ();
    private static Convert convert = new Convert ();

    public TheFunction ()
    {
	for (final Object[] ttc : TYPE_TO_CLASS)
	{
	    typeToClass.put ((String)ttc[0], (Class<?>)ttc[1]);
	}
    }

    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	visitor.visitValue (expression.get (2));
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final Object type = expression.get (1);
	final Object arg = expression.get (2);
	// (setq system.showBytecode t)
	// (define foo () byte:3)
	// Need to allow narrowing conversions here
	if (type instanceof Symbol)
	{
	    final Symbol t = (Symbol)type;
	    final Class<?> toClass = typeToClass.get (t.getName ());

	    final CompileResultSet rs = context.compile (arg, resultDesired);
	    context.convert (rs, toClass, true, false);
	    final LabelNode l1 = new LabelNode ();
	    return new CompileResultSet (new ExplicitCompileResult (l1, toClass));
	}
	// Narrowing has not been implemented below this line
	else if (type instanceof Class)
	{
	    final Class<?> c = (Class<?>)type;
	    final CompileResultSet rs = context.compile (arg, resultDesired);
	    context.convert (rs, c, true, false);
	    final LabelNode l1 = new LabelNode ();
	    return new CompileResultSet (new ExplicitCompileResult (l1, c));
	}
	else if (type instanceof String)
	{
	    final String t = (String)type;
	    try
	    {
		final Class<?> c = Class.forName (t);
		final CompileResultSet rs = context.compile (arg, resultDesired);
		context.convert (rs, c, true, false);
		final LabelNode l1 = new LabelNode ();
		return new CompileResultSet (new ExplicitCompileResult (l1, c));
	    }
	    catch (final ClassNotFoundException e)
	    {
	    }
	    if (t.indexOf (".") < 0)
	    {
		try
		{
		    final Class<?> c = Class.forName ("java.lang." + t);
		    final CompileResultSet rs = context.compile (arg, resultDesired);
		    context.convert (rs, c, true, false);
		    final LabelNode l1 = new LabelNode ();
		    return new CompileResultSet (new ExplicitCompileResult (l1, c));
		}
		catch (final ClassNotFoundException e)
		{
		}
	    }
	}
	throw new UnsupportedOperationException ("Can't convert " + arg + " to " + type);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	final Object type = e.get (1);
	final Object arg = e.get (2);

	compileThe (generator, mv, type, arg, valueClass, allowNarrowing, liberalTruth);
    }

    private void compileThe (final CompilerGenerator generator, final GeneratorAdapter mv, final Object type, final Object arg,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (setq system.showBytecode t)
	// (define foo () byte:3)
	// Need to allow narrowing conversions here
	if (type instanceof Symbol)
	{
	    final Symbol t = (Symbol)type;
	    if (t.is ("byte"))
	    {
		// (define byte:foo () (the byte int:3))
		// (define byte:foo () byte:int:3)
		// (d (foo))
		generator.compileExpression (mv, arg, byte.class, true, false);
		convert.convert (mv, byte.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("char"))
	    {
		generator.compileExpression (mv, arg, int.class, true, false);
		// mv.visitInsn (I2C); // Narrow
		convert.convert (mv, char.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("short"))
	    {
		// (define short:foo () (the short int:3))
		generator.compileExpression (mv, arg, int.class, true, false);
		convert.convert (mv, short.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("int"))
	    {
		generator.compileExpression (mv, arg, int.class, true, false);
		convert.convert (mv, int.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("long"))
	    {
		// (define long:foo () int:3) ; Widening
		// (define long:foo () float:3.3) ; Error needs cast
		// (define long:foo () long:float:3.3) ; Explicit cast
		generator.compileExpression (mv, arg, long.class, true, false);
		convert.convert (mv, long.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("float"))
	    {
		// NOT WORKING
		// mv.visitLdcInsn ((float)6.9);
		generator.compileExpression (mv, arg, float.class, true, false);
		convert.convert (mv, float.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("double"))
	    {
		generator.compileExpression (mv, arg, double.class, true, false);
		convert.convert (mv, double.class, valueClass, true, false);
		return;
	    }
	    compileThe (generator, mv, t.getName (), arg, valueClass, allowNarrowing, liberalTruth);
	    return;
	}
	// Narrowing has not been implemented below this line
	else if (type instanceof Class)
	{
	    final Class<?> c = (Class<?>)type;
	    convert.convert (mv, Object.class, c, true, false);
	    convert.convert (mv, c, valueClass, false, false);
	    return;
	}
	if (type instanceof String)
	{
	    final String t = (String)type;
	    try
	    {
		final Class<?> c = Class.forName (t);
		convert.convert (mv, c, valueClass, false, false);
		return;
	    }
	    catch (final ClassNotFoundException e)
	    {
	    }
	    if (t.indexOf (".") < 0)
	    {
		try
		{
		    final Class<?> c = Class.forName ("java.lang." + t);
		    convert.convert (mv, c, valueClass, false, false);
		    return;
		}
		catch (final ClassNotFoundException e)
		{
		}
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
