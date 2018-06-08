
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.asm.instructions.*;
import lisp.cc.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class DotimesFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
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
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo () (dotimes (i 10) (printf "foo")))
	// (define foo (int:n) (dotimes (i n) (printf "foo %s%n" i)))

	final LabelNode l0 = new LabelNode ();
	final LabelNode l1 = new LabelNode ();

	final LispList control = expression.getSublist (1);
	final Symbol var = control.head (); // always an int, not need to declare
	final TreeCompilerContext innerContext = context.bindVariable (var, int.class);
	final LocalBinding binding = innerContext.getLocalVariableBinding (var);
	final int countRef = binding.getLocalRef ();
	innerContext.add (new InsnNode (ICONST_0));
	innerContext.add (new VarInsnNode (ISTORE, countRef));

	final Object countExpression = control.get (1);
	final CompileResultSet repeatCount = context.compile (countExpression, true);
	context.convert (repeatCount, int.class, false, false);
	{
	    // Current index is in local, repeat limit is on stack
	    innerContext.add (l1);
	    innerContext.add (new InsnNode (DUP));
	    innerContext.add (new VarInsnNode (ILOAD, countRef));
	    innerContext.add (new JumpInsnNode (IF_ICMPLT, l0));

	    for (int i = 2; i < expression.size (); i++)
	    {
		final CompileResultSet r = innerContext.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		innerContext.convert (r, void.class, false, false);
	    }
	    innerContext.add (new IincInsnNode (countRef, 1));
	    innerContext.add (new JumpInsnNode (GOTO, l1));

	}
	context.add (l0);
	context.add (new InsnNode (POP));
	// Always return false
	final LabelNode ll = new LabelNode ();
	context.add (new JumpInsnNode (GOTO, ll));
	return new CompileResultSet (new ImplicitCompileResult (ll, false));
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
	generator.compileExpression (mv, count, Object.class /* TODO */, false, false);
	mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	// Leave repeat count on the stack

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
	// Stack repeatCount
	// <body code goes here>
	for (int i = 2; i < e.size (); i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
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
	// Stack repeatCount

	mv.visitInsn (DUP); // Dup count
	// Stack repeatCount, repeatCount
	mv.visitVarInsn (ALOAD, iterationRef);
	// Stack iteration, repeatCount, repeatCount
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	mv.visitJumpInsn (IF_ICMPGT, l2);
	// Stack repeatCount

	mv.visitInsn (POP); // Remove repeat count
	generator.setLocalBindingContext (savedLocalVariableMap);
	// Return false
	mv.visitInsn (ICONST_0);
	generator.convert (mv, boolean.class, valueType, allowNarrowing, liberalTruth);
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
