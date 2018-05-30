
package lisp.special;

import java.util.logging.Logger;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.cc.*;
import lisp.eval.*;
import lisp.util.LogString;

public class Setq extends Definer implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Setq.class.getName ());

    /**
     * Interpreter for setq statements.
     *
     * @param interpreter The interpreter used to evaluate forms.
     * @param arguments The symbol and value form.
     * @return The new value.
     */
    @DefineLisp (special = true)
    public Object setq (final LexicalContext context, final Symbol symbol, final Object form) throws Exception
    {
	final Object value = context.eval (form);
	context.set (symbol, value);
	return value;
    }

    @DefineLisp
    public Object symbolValue (final Symbol arg)
    {
	return arg.getValue ();
    }

    /**
     * Set an evaluated symbol value. This is intended for use in macros that produce a form which
     * will be evaluated to determine the symbol to modify.
     */
    @DefineLisp (name = "setSymbolValue")
    public Object setSymbolValue (final Symbol symbol, final Object value)
    {
	symbol.setValue (value);
	return value;
    }

    @DefineLisp (special = true, name = "setq", compiler = true)
    public void compileSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (setq x 3))
	// (define foo (x) (setq x 3) x)
	// (define foo (x) (setq a x))
	// (define foo (x) (let ((a 3)) (setq a (+ a x)) a))
	final Symbol symbol = (Symbol)e.get (1);
	final LocalBinding lb = generator.getLocalVariableBinding (symbol);
	if (lb != null)
	{
	    LOGGER.finer (new LogString ("Setq local %s (%d)", symbol, lb));
	    compileLocalSetq (generator, mv, lb.getClass (), lb.getLocalRef (), e.get (2), valueType, allowNarrowing,
	            liberalTruth);
	}
	else if (generator.isMethodArg (symbol))
	{
	    // Parameter reference
	    // [TODO] If we can determine the type, use that information.
	    final int argIndex = generator.getMethodArgIndex (symbol);
	    final Class<?> argClass = generator.getMethodArgClass (symbol);
	    LOGGER.finer (new LogString ("Setq parameter %s (%d)", symbol, argIndex));
	    compileArgSetq (generator, mv, argClass, argIndex, e.get (2), valueType, allowNarrowing, liberalTruth);
	}
	else
	{
	    generator.addSymbolReference (symbol);
	    // if (!symbolReferences.contains (symbol))
	    // {
	    // symbolReferences.add (symbol);
	    // }
	    LOGGER.finer (new LogString ("Symbol assignment to %s", symbol));
	    // [TODO] If the symbol valueCell is constant, use the current value.
	    // [TODO] If the valueCell is a TypedValueCell, use the type information.
	    mv.visitVarInsn (ALOAD, 0);
	    final String classInternalName = generator.getClassType ().getInternalName ();
	    mv.visitFieldInsn (GETFIELD, classInternalName, generator.createJavaSymbolName (symbol), "Llisp/Symbol;");
	    generator.compileExpression (mv, e.get (2), Object.class /* TODO */, false, false);
	    if (valueType != null)
	    {
		// Copy the expression value so it becomes the return value
		mv.visitInsn (DUP_X1);
	    }

	    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V", false);
	    // Return the expression value
	    generator.addGlobalReference (symbol);
	    // if (!globalReferences.contains (symbol))
	    // {
	    // globalReferences.add (symbol);
	    // LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
	    // }
	    if (valueType != null)
	    {
		generator.coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
	    }
	}
    }

    private void compileArgSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final Class<?> varClass,
            final int localRef, final Object expr, final Class<?> valueType, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (valueType == null)
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.storeArg (localRef);
	}
	else
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.visitInsn (DUP);
	    mv.storeArg (localRef);
	    generator.coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
	}
    }

    private void compileLocalSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final Class<?> varClass,
            final int localRef, final Object expr, final Class<?> valueType, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (valueType == null)
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.storeLocal (localRef);
	    // mv.visitVarInsn (ASTORE, localRef);
	}
	else
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.visitInsn (DUP);
	    mv.storeLocal (localRef);
	    // mv.visitVarInsn (ASTORE, localRef);
	    generator.coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
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
