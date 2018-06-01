
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.Package;
import lisp.Symbol;
import lisp.cc.*;
import lisp.symbol.*;

public class CondFunction extends LispFunction implements Opcodes
{
    public CondFunction (final Symbol symbol)
    {
	super (symbol);
    }

    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	for (int i = 1; i < expression.size (); i++)
	{
	    final LispList clause = (LispList)expression.get (i);
	    if (clause.size () == 1)
	    {
		visitor.visitBooleanValue (clause.get (0));
	    }
	    else
	    {
		visitor.visitBoolean (clause.get (0));
		for (int j = 1; j < clause.size () - 1; j++)
		{
		    visitor.visitIgnored (clause.get (j));
		}
		visitor.visitValue (clause.last ());
	    }
	}
	visitor.visitEnd (expression);
	// May return a default value
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueClass == null)
	{
	    compileVoidCond (generator, mv, expression);
	}
	else if (valueClass.equals (boolean.class))
	{
	    compileBooleanCond (generator, mv, expression);
	}
	else
	{
	    compileCond (generator, mv, expression, valueClass, allowNarrowing, liberalTruth);
	}
    }

    /** Case where no return value is required. */
    private void compileVoidCond (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (x) (cond ((= x 1) (printf "one%n")) ((= x 2)(printf "two%n"))) 'done)

	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final int size = clause.size ();
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();

	    generator.compileExpression (mv, key, boolean.class, false, false);
	    mv.visitJumpInsn (IFEQ, l2);

	    // Clause selected
	    for (int j = 1; j < size; j++)
	    {
		generator.compileExpression (mv, clause.get (j), null, false, false);
	    }
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l2);
	}
	// Return result
	mv.visitLabel (l1);
    }

    /** Case where only a boolean value is required. */
    // (define foo (x) (when (cond ((= x 1)) ((= x 2) false) ((= x 3) true)) (printf "when%n")))
    // (define foo (x) (when (cond ((= x 1)) ((= x 2) false) ((= x 3))) (printf "when%n")))
    private void compileBooleanCond (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final int size = clause.size ();
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();

	    generator.compileExpression (mv, key, boolean.class, false, false);
	    mv.visitJumpInsn (IFEQ, l2);

	    // Clause selected
	    if (size > 1)
	    {
		for (int j = 1; j < size - 1; j++)
		{
		    generator.compileExpression (mv, clause.get (j), null, false, false);
		}
		generator.compileExpression (mv, clause.get (size - 1), boolean.class, false, false);
	    }
	    else
	    {
		mv.visitLdcInsn (true);
	    }
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l2);
	}
	// Return result
	mv.visitLdcInsn (false);
	mv.visitLabel (l1);
    }

    /** Compile a cond expression where the value will be used. */
    private void compileCond (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (setq showBytecode t)
	// (define foo (x) (cond (x 1)))
	// (define foo (x) (cond ((= x 1) 'alpha)))
	// (define foo (x) (cond ((= x 1) 'alpha) ((= x 2) 'beta) ((= x 3) 'gamma) (true 'delta)))
	if (valueClass == null)
	{
	    throw new Error ("Use compileVoidCond when valueType is null");
	}
	final Package system = PackageFactory.getSystemPackage ();
	final Symbol var = system.internSymbol ("result").gensym ();

	// Setup return value in a local variable

	// Store as an object until return time
	final int resultRef = mv.newLocal (Boxer.OBJECT_TYPE);
	final LocalBinding lb = new LocalBinding (var, Object.class, resultRef);

	final Map<Symbol, LocalBinding> savedBindings = generator.getLocalBindingContext ();
	// final Map<Symbol, LocalBinding> savedBindings = localVariableMap;
	final Map<Symbol, LocalBinding> localVariableMap = new HashMap<Symbol, LocalBinding> (savedBindings);
	localVariableMap.put (var, lb);
	generator.setLocalBindingContext (localVariableMap);
	generator.pushDefaultValue (mv, Object.class, false);
	mv.storeLocal (resultRef);

	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    // Stack is empty
	    final LispList clause = (LispList)e.get (i);
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();
	    final Label l3 = new Label ();
	    final Label l4 = new Label ();
	    if (clause.size () == 1)
	    {
		generator.compileExpression (mv, key, Object.class, false, true);
		mv.visitInsn (DUP);
		mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
		mv.visitJumpInsn (IFEQ, l2); // Check for boolean

		mv.visitInsn (DUP);
		mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
		mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
		mv.visitJumpInsn (IFEQ, l3); // Proceed to next clause

		// Clause selected and value of key expression is return value
		mv.visitLabel (l2);
	    }
	    else
	    {
		// No need to save value of key expression
		generator.compileExpression (mv, key, boolean.class, false, true);
		mv.visitJumpInsn (IFEQ, l4); // Proceed to next clause

		// Clause selected
		mv.visitLabel (l2);

		// One entry on stack
		if (clause.size () > 1)
		{
		    for (int j = 1; j < clause.size () - 1; j++)
		    {
			generator.compileExpression (mv, clause.get (j), null, false, false);
		    }
		    generator.compileExpression (mv, clause.last (), valueClass, allowNarrowing, liberalTruth);
		}
	    }

	    mv.storeLocal (resultRef);
	    // Stack is empty
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l3);
	    mv.visitInsn (POP);
	    mv.visitLabel (l4);
	    // Stack is empty
	}
	// Return result
	mv.visitLabel (l1);
	mv.loadLocal (resultRef);
	// generator.coerceRequiredX (mv, valueClass);
	// generator.convert (mv, Object.class, valueClass, allowNarrowing, liberalTruth);
	// localVariableMap = savedBindings;
	generator.setLocalBindingContext (savedBindings);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getSymbol ());
	buffer.append (">");
	return buffer.toString ();
    }
}
