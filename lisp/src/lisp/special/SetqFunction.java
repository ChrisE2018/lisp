
package lisp.special;

import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.asm.instructions.*;
import lisp.cc.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;
import lisp.util.LogString;

public class SetqFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    private static final Logger LOGGER = Logger.getLogger (SetqFunction.class.getName ());

    private static JavaName javaName = new JavaName ();

    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	final Symbol symbol = (Symbol)expression.get (1);
	visitor.visitSymbolSet (symbol);
	visitor.visitValue (expression.get (2));
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final Symbol symbol = (Symbol)expression.get (1);
	final LocalBinding lb = context.getLocalVariableBinding (symbol);
	if (lb != null)
	{
	    // The following code will generate ISTORE/ILOAD which is optimized away.
	    // (setq system.removeStoreLoad false) to see it.
	    // (define foo (int:x) (setq x 3 ) x)
	    final int localRef = lb.getLocalRef ();
	    final Class<?> varClass = lb.getVariableClass ();
	    final Type varType = lb.getType ();
	    final CompileResultSet results = context.compile (expression.get (2), true);
	    context.convert (results, varClass, false, false);
	    if (resultDesired)
	    {
		if (varType.getSize () == 1)
		{
		    context.add (new InsnNode (DUP));
		}
		else if (varType.getSize () == 2)
		{
		    context.add (new InsnNode (DUP2));
		}
		else
		{
		    throw new Error ("Invalid size");
		}
	    }
	    context.add (new VarInsnNode (varType.getOpcode (ISTORE), localRef));
	    final LabelNode ll = new LabelNode ();
	    context.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResultSet (new ExplicitCompileResult (ll, resultDesired ? varClass : void.class));
	}
	else
	{
	    // FIXME If the symbol valueCell is constant, use the current value.
	    // TODO If the valueCell is a TypedValueCell, use the type information.
	    // global
	    final TreeCompilerInterface compiler = context.getTreeCompiler ();
	    compiler.addSymbolReference (symbol);
	    compiler.addGlobalReference (symbol);
	    final String javaSymbolName = javaName.createJavaSymbolName (symbol);
	    LOGGER.finer (new LogString ("Global assignment to %s as %s", symbol, javaSymbolName));
	    final String classInternalName = compiler.getClassType ().getInternalName ();
	    context.add (new VarInsnNode (ALOAD, 0));
	    context.add (new FieldInsnNode (GETFIELD, classInternalName, javaSymbolName, "Llisp/Symbol;"));
	    final CompileResultSet results = context.compile (expression.get (2), true);
	    context.convert (results, Object.class, false, false);
	    if (resultDesired)
	    {
		// Copy the expression value so it becomes the return value
		context.add (new InsnNode (DUP_X1));
	    }
	    context.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V", false));
	    final LabelNode ll = new LabelNode ();
	    context.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResultSet (new ExplicitCompileResult (ll, resultDesired ? Object.class : void.class));
	}
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expr,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (setq x 3))
	// (define foo (x) (setq x 3) x)
	// (define foo (x) (setq a x))
	// (define foo (x) (let ((a 3)) (setq a (+ a x)) a))
	final Symbol symbol = (Symbol)expr.get (1);
	final LocalBinding lb = generator.getLocalVariableBinding (symbol);
	if (lb != null)
	{
	    LOGGER.finer (new LogString ("Setq local %s (%d)", symbol, lb));
	    compileLocalSetq (generator, mv, lb, expr.get (2), valueClass, allowNarrowing, liberalTruth);
	}
	else if (generator.isMethodArg (symbol))
	{
	    // Parameter reference
	    LOGGER.finer (new LogString ("Setq parameter %s (%d)", symbol, lb));
	    compileArgSetq (generator, mv, symbol, expr.get (2), valueClass, allowNarrowing, liberalTruth);
	}
	else
	{
	    LOGGER.finer (new LogString ("Setq global %s (%d)", symbol, lb));
	    compileGlobalSetq (generator, mv, symbol, expr.get (2), valueClass, allowNarrowing, liberalTruth);
	}
    }

    /** Compile a setq that modifies a local variable. */
    private void compileLocalSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final LocalBinding lb,
            final Object expr, final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	final int localRef = lb.getLocalRef ();
	final Class<?> varClass = lb.getVariableClass ();
	if (valueClass == null)
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.storeLocal (localRef);
	}
	else
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.visitInsn (DUP);
	    mv.storeLocal (localRef);
	    generator.convert (mv, varClass, valueClass, allowNarrowing, liberalTruth);
	}
    }

    /** Compile a setq that modifies a function argument. */
    private void compileArgSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final Symbol symbol,
            final Object expr, final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// TODO If we can determine the type, use that information.
	final int localRef = generator.getMethodArgIndex (symbol);
	final Class<?> varClass = generator.getMethodArgClass (symbol);
	if (valueClass == null)
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.storeArg (localRef);
	}
	else
	{
	    generator.compileExpression (mv, expr, varClass, false, false);
	    mv.visitInsn (DUP);
	    mv.storeArg (localRef);
	    generator.convert (mv, varClass, valueClass, allowNarrowing, liberalTruth);
	}
    }

    private void compileGlobalSetq (final CompilerGenerator generator, final GeneratorAdapter mv, final Symbol symbol,
            final Object valueExpr, final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	generator.addSymbolReference (symbol);
	// TODO If the symbol valueCell is constant, use the current value.
	// TODO If the valueCell is a TypedValueCell, use the type information.
	mv.visitVarInsn (ALOAD, 0);
	final String classInternalName = generator.getClassType ().getInternalName ();
	mv.visitFieldInsn (GETFIELD, classInternalName, generator.createJavaSymbolName (symbol), "Llisp/Symbol;");
	generator.compileExpression (mv, valueExpr, Object.class, false, false);
	if (valueClass != null)
	{
	    // Copy the expression value so it becomes the return value
	    mv.visitInsn (DUP_X1);
	}

	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V", false);
	// Return the expression value
	generator.addGlobalReference (symbol);
	if (valueClass != null)
	{
	    generator.convert (mv, Object.class, valueClass, allowNarrowing, liberalTruth);
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
