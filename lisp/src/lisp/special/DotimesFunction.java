
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.*;
import lisp.symbol.*;

public class DotimesFunction extends LispFunction implements Opcodes
{
    public DotimesFunction (final Symbol symbol)
    {
	super (symbol);
    }

    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	final LispList clause = (LispList)expression.get (1);
	// [TODO] visit variable binding
	visitor.visitInteger (clause.get (1));
	for (int i = 2; i < expression.size () - 1; i++)
	{
	    visitor.visitIgnored (expression.get (i));
	}
	visitor.visitValue (expression.last ());
	visitor.visitEnd (expression);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo () (dotimes (i 3) 0))
	// (define foo () (dotimes (i 3) 0) 5)
	// (define foo (n) (dotimes (i n) 0))
	// (define foo () (dotimes (i 3) (printf "i = %s %n" i)))
	// (define foo (n) (dotimes (i n) (printf "i = %s %n" i)))

	// Compute repeat count
	final List<?> control = (List<?>)e.get (1);
	final Object count = control.get (1);
	// [TODO] Currently always compiles for Object result. Need to analyze the body and
	// determine if the value is actually referenced.
	generator.compileExpression (mv, count, Object.class /* TODO */, false, false);
	mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	// Leave repeat count on the stack

	// Push default return value onto the stack
	generator.pushDefaultValue (mv, valueType, true);
	// Stack [returnValue], repeatCount

	// Put iteration number into local variable
	final Map<Symbol, LocalBinding> savedLocalVariableMap = generator.getLocalBindingContext ();
	final Map<Symbol, LocalBinding> localVariableMap = new LinkedHashMap<Symbol, LocalBinding> (savedLocalVariableMap);
	generator.setLocalBindingContext (localVariableMap);
	// Create a local variable to hold the iteration number.
	// This is always stored in boxed format so body code can reference it.
	// Should be able to store this as an int if the body code can use it that way.
	final Type type = Boxer.INTEGER_TYPE;
	final int iterationRef = mv.newLocal (type);
	final Symbol var = (Symbol)control.get (0);
	final LocalBinding lb = new LocalBinding (var, Integer.class, iterationRef);
	localVariableMap.put (var, lb);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	mv.visitVarInsn (ASTORE, iterationRef);

	// Jump to termination test
	final Label l1 = new Label ();
	mv.visitJumpInsn (GOTO, l1);

	// Start of iteration body
	final Label l2 = new Label ();
	mv.visitLabel (l2);
	// Stack repeatCount, [returnValue]
	if (valueType != null)
	{
	    mv.visitInsn (SWAP); // Save repeat count
	}
	// <body code goes here>
	for (int i = 2; i < e.size (); i++)
	{
	    if (valueType != null)
	    {
		mv.visitInsn (POP);
	    }
	    generator.compileExpression (mv, e.get (i), valueType, false, false);
	}

	// Loop increment
	mv.visitVarInsn (ALOAD, iterationRef);
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	mv.visitInsn (ICONST_1);
	mv.visitInsn (IADD);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	mv.visitVarInsn (ASTORE, iterationRef);

	// // Termination test
	mv.visitLabel (l1);
	// Stack [returnValue], repeatCount

	if (valueType != null) // ***ADDED
	{
	    mv.visitInsn (SWAP);
	}
	// Stack repeatCount, [returnValue]
	mv.visitInsn (DUP); // Dup count
	// Stack repeatCount, repeatCount, [returnValue]
	mv.visitVarInsn (ALOAD, iterationRef);
	// Stack iteration, repeatCount, repeatCount, [returnValue]
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	mv.visitJumpInsn (IF_ICMPGT, l2);
	// Stack repeatCount, [returnValue]

	mv.visitInsn (POP); // Remove repeat count
	// coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
	// Return last body value
	generator.setLocalBindingContext (savedLocalVariableMap);
	// localVariableMap = savedLocalVariableMap;
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
