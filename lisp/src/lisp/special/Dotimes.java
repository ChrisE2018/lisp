
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.*;
import lisp.eval.*;

public class Dotimes extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "dotimes")
    public Object dotimes (final LexicalContext context, final List<?> control, final Object... arguments) throws Exception
    {
	Object result = true;
	final Symbol var = (Symbol)control.get (0);
	final int n = (Integer)context.eval (control.get (1));
	final LexicalContext subcontext = new LexicalContext (context);
	subcontext.bind (var, 0);
	for (int j = 0; j < n; j++)
	{
	    subcontext.set (var, j);
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = subcontext.eval (arg);
	    }
	}
	return result;
    }

    @DefineLisp (special = true, name = "dotimes", compiler = true)
    public void compileDotimes (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
	final LocalBinding lb = new LocalBinding (var, type, iterationRef);
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
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}