
package lisp.special;

import java.util.List;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.*;

import lisp.LispList;
import lisp.cc.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class OrFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	for (int i = 1; i < expression.size (); i++)
	{
	    // [TODO] If the expression always returns false, ignore it.
	    visitor.visitValue (expression.get (i));
	}
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	if (!resultDesired)
	{
	    compile2void (context, expression);
	    return VOID_RETURN;
	}
	else if (expression.size () == 1)
	{
	    // case (or)
	    return new CompileResultSet (new ImplicitCompileResult (null, false));
	}
	else if (expression.size () == 2)
	{
	    // case (or x)
	    return context.compile (expression.get (1), resultDesired);
	}
	else
	{
	    return compileOr (context, expression, resultDesired);
	}
    }

    private CompileResultSet compileOr (final TreeCompilerContext context, final LispList e, final boolean resultDesired)
    {
	// (define foo (a b) (or))
	// (define foo (a b) (or a b))
	// Fall through to implicit true
	final CompileResultSet result = new CompileResultSet ();
	final LabelNode lTrue = new LabelNode (); // Implicit true
	final LabelNode lPopTrue = new LabelNode (); // pop true
	final LabelNode lexit = new LabelNode ();

	boolean lTrueUsed = false;
	final boolean lPopTrueUsed = false;

	boolean stackOccupied = false;
	for (int i = 1; i < e.size () - 1; i++)
	{
	    // Jump here to check next conjunct
	    final LabelNode lNext = new LabelNode ();
	    final CompileResultSet r = context.compile (e.get (i), true);
	    final List<CompileResult> crl = r.getResults ();
	    for (int j = 0; j < crl.size (); j++)
	    {
		final CompileResult cr = crl.get (j);
		context.add (cr.getLabel ());
		if (cr instanceof ImplicitCompileResult)
		{
		    final ImplicitCompileResult icr = ((ImplicitCompileResult)cr);
		    if (icr.getValue ().equals (Boolean.FALSE))
		    {
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else
		    {
			lTrueUsed = true;
			context.add (new JumpInsnNode (GOTO, lTrue));
		    }
		}
		else
		{
		    final ExplicitCompileResult ecr = (ExplicitCompileResult)cr;
		    final Class<?> resultClass = ecr.getResultClass ();
		    if (boolean.class.equals (resultClass))
		    {
			lTrueUsed = true;
			context.add (new JumpInsnNode (IFNE, lTrue));
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (Boolean.class.equals (resultClass))
		    {
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			lTrueUsed = true;
			context.add (new JumpInsnNode (IFNE, lTrue));
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (Boolean.class.isAssignableFrom (resultClass))
		    {
			stackOccupied = true;
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			context.add (new JumpInsnNode (IFNE, lTrue));
			lTrueUsed = true;
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (long.class.equals (resultClass) || double.class.equals (resultClass))
		    {
			// Handle primitive long and double differently to keep stack size right
			context.add (new InsnNode (POP2));
			context.add (new JumpInsnNode (GOTO, lTrue));
		    }
		    else
		    {
			stackOccupied = true;
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (INSTANCEOF, "java/lang/Boolean"));
			// Not a Boolean should be returned immediately
			context.add (new JumpInsnNode (IFEQ, lexit));
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			context.add (new JumpInsnNode (IFNE, lexit));
			result.addExplicitCompileResult (null, resultClass);
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		}
	    }
	    context.add (lNext);
	    if (stackOccupied)
	    {
		context.add (new InsnNode (POP));
	    }
	}
	// If we get here, just return the value
	final CompileResultSet r = context.compile (e.last (), resultDesired);
	final List<CompileResult> crl = r.getResults ();
	for (int j = 0; j < crl.size () - 1; j++)
	{
	    final CompileResult cr = crl.get (j);
	    if (cr.getLabel () != null)
	    {
		result.getResults ().add (cr);
	    }
	    else
	    {
		context.add (new JumpInsnNode (GOTO, lexit));
	    }
	}
	// Put the last one in
	final CompileResult last = crl.get (crl.size () - 1);
	result.getResults ().add (last);
	if (last.getLabel () == null)
	{
	    if (lPopTrueUsed)
	    {
		context.add (new JumpInsnNode (GOTO, lexit));
	    }
	}

	if (lPopTrueUsed)
	{
	    context.add (lPopTrue);
	    context.add (new InsnNode (POP));
	    lTrueUsed = true;
	    context.add (new JumpInsnNode (GOTO, lTrue));
	}
	if (lTrueUsed)
	{
	    result.addImplicitCompileResult (lTrue, true);
	}
	context.add (lexit);
	return result;
    }

    /** Compile an 'or' expression whose value is only used as a boolean */
    @SuppressWarnings ("unused")
    private void compileBooleanAnd (final TreeCompilerContext context, final LispList e)
    {
	// (define foo (a b) (if (or a b) 1 2))
	final LabelNode l1 = new LabelNode ();
	for (int i = 1; i < e.size (); i++)
	{
	    final CompileResultSet r = context.compile (e.get (i), true);
	    context.convert (r, boolean.class, false, true);
	    context.add (new JumpInsnNode (IFNE, l1));
	}
	// False case
	final LabelNode l2 = new LabelNode ();
	context.add (new InsnNode (ICONST_0));
	context.add (new JumpInsnNode (GOTO, l2));

	// True case
	context.add (l1);
	context.add (new InsnNode (ICONST_1));

	// Jump here after true case or fall through after false.
	// Return final value.
	context.add (l2);
    }

    /** Compile an 'or' expression whose value will be ignored. */
    private void compile2void (final TreeCompilerContext context, final LispList e)
    {
	// (define foo (a b) (or) 1)
	// (define foo (a b) (or a b) 2)
	// (define boolean:foo (a b) (or) 1)
	if (e.size () > 0)
	{
	    final LabelNode l1 = new LabelNode ();
	    for (int i = 1; i < e.size (); i++)
	    {
		final CompileResultSet crs = context.compile (e.get (i), false);
		context.convert (crs, boolean.class, false, true);
		context.add (new JumpInsnNode (IFNE, l1));
	    }
	    context.add (l1);
	}
    }

    // OLD VERSION
    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueClass == null)
	{
	    compileVoidOr (generator, mv, expression);
	}
	else if (valueClass.equals (boolean.class))
	{
	    compileBooleanOr (generator, mv, expression);
	}
	else
	{
	    compileOr (generator, mv, expression, valueClass, allowNarrowing, liberalTruth);
	}
    }

    /** Compile an 'or' expression whose value will be ignored. */
    private void compileVoidOr (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (a b) (or) 2)
	// (define foo (a b) (or a b) 3)
	if (e.size () > 0)
	{
	    final Label l1 = new Label ();
	    for (int i = 1; i < e.size (); i++)
	    {
		generator.compileExpression (mv, e.get (i), boolean.class, false, true);
		mv.visitJumpInsn (IFNE, l1);
	    }
	    mv.visitLabel (l1);
	}
    }

    /** Compile an 'or' expression whose value is only used as a boolean */
    private void compileBooleanOr (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (a b) (if (or a b) 1 2))
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    generator.compileExpression (mv, e.get (i), boolean.class, false, true);
	    mv.visitJumpInsn (IFNE, l1);
	}
	// False case
	final Label l2 = new Label ();
	mv.visitInsn (ICONST_0);
	mv.visitJumpInsn (GOTO, l2);

	// True case
	mv.visitLabel (l1);
	mv.visitInsn (ICONST_1);

	// Jump here after false case or fall through after true.
	// Return final value.
	mv.visitLabel (l2);
    }

    private void compileOr (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (a b) (or))
	// (foo 1 2)
	// (define foo (a b) (or a b))
	final Label l1 = new Label ();
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	for (int i = 1; i < e.size (); i++)
	{
	    mv.visitInsn (POP);
	    generator.compileExpression (mv, e.get (i), Object.class, false, true);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	    mv.visitJumpInsn (IFEQ, l1);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	    mv.visitJumpInsn (IFNE, l1);
	}
	// False case
	mv.visitInsn (POP);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);

	// Jump here for true case or fall through in false case
	mv.visitLabel (l1);
	// (define int:bar (a b) (or a b))
	// generator.coerceRequiredX (mv, valueType);
	generator.convert (mv, Object.class, valueClass, allowNarrowing, liberalTruth);
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
