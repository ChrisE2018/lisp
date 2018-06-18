
package lisp.special;

import java.util.List;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.symbol.LispVisitor;

public class AndFunction implements LispCCFunction, Opcodes, LispTreeWalker, LispTreeFunction
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	for (int i = 1; i < expression.size (); i++)
	{
	    // CONSIDER If the expression always returns non-false, ignore it.
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
	    // case (and)
	    final LabelNode ll = new LabelNode ();
	    context.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResultSet (new ImplicitCompileResult (ll, true));
	}
	else if (expression.size () == 2)
	{
	    // case (and x)
	    return context.compile (expression.get (1), resultDesired);
	}
	else
	{
	    return compileAnd (context, expression, resultDesired);
	}
    }

    /**
     * Compile a general and expression. This implementation is very complex because it is the first
     * function definition built using the new compiler framework. The methods context.convert and
     * context.convertIfFalse were derived from this code and could be used to produce a much
     * simpler implementation. However, this works and may be valuble as an example of how to
     * implement such a compiler extension "from scratch".
     *
     * @param context
     * @param e
     * @param resultDesired
     * @return
     */
    private CompileResultSet compileAnd (final TreeCompilerContext context, final LispList e, final boolean resultDesired)
    {
	// (define foo (a b) (and))
	// (define foo (a b) (and a b))
	// Fall through to implicit true
	final CompileResultSet result = new CompileResultSet ();
	// final LabelNodeSet lTrue = new LabelNodeSet (); // Implicit true
	final LabelNode lFalse = new LabelNode (); // Implicit false
	final LabelNode lPopFalse = new LabelNode (); // pop false

	boolean lFalseUsed = false;
	boolean lPopFalseUsed = false;

	boolean stackOccupied = false;
	for (int i = 1; i < e.size () - 1; i++)
	{
	    final LabelNode lNext = new LabelNode ();
	    final CompileResultSet r = context.compile (e.get (i), true);
	    final List<CompileResult> crl = r.getResults ();
	    for (int j = 0; j < crl.size (); j++)
	    {
		final CompileResult cr = crl.get (j);
		context.add (cr.getLabels ());
		if (cr instanceof ImplicitCompileResult)
		{
		    final ImplicitCompileResult icr = ((ImplicitCompileResult)cr);
		    if (icr.getValue ().equals (Boolean.FALSE))
		    {
			lFalseUsed = true;
			// If we found a hard false result we can delete the remaining clauses
			context.add (new JumpInsnNode (GOTO, lFalse));
		    }
		    else
		    {
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		}
		else
		{
		    final ExplicitCompileResult ecr = (ExplicitCompileResult)cr;
		    final Class<?> resultClass = ecr.getResultClass ();
		    if (boolean.class.equals (resultClass))
		    {
			lFalseUsed = true;
			context.add (new JumpInsnNode (IFEQ, lFalse));
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (Boolean.class.equals (resultClass))
		    {
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			lFalseUsed = true;
			context.add (new JumpInsnNode (IFEQ, lFalse));
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (Boolean.class.isAssignableFrom (resultClass))
		    {
			stackOccupied = true;
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			context.add (new JumpInsnNode (IFEQ, lFalse));
			lFalseUsed = true;
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else if (long.class.equals (resultClass) || double.class.equals (resultClass))
		    {
			context.add (new InsnNode (POP2));
			context.add (new JumpInsnNode (GOTO, lNext));
		    }
		    else
		    {
			stackOccupied = true;
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (INSTANCEOF, "java/lang/Boolean"));
			context.add (new JumpInsnNode (IFEQ, lNext));
			context.add (new InsnNode (DUP));
			context.add (new TypeInsnNode (CHECKCAST, "java/lang/Boolean"));
			context.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
			lPopFalseUsed = true;
			context.add (new JumpInsnNode (IFEQ, lPopFalse));
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
	final LabelNode lExit = new LabelNode ();
	final CompileResultSet r = context.compile (e.last (), resultDesired);
	final List<CompileResult> crl = r.getResults ();
	for (int j = 0; j < crl.size (); j++)
	{
	    final CompileResult cr = crl.get (j);
	    result.add (cr);
	}
	context.add (new JumpInsnNode (GOTO, lExit));

	if (lPopFalseUsed)
	{
	    context.add (lPopFalse);
	    context.add (new InsnNode (POP));
	    lFalseUsed = true;
	    context.add (new JumpInsnNode (GOTO, lFalse));
	}
	if (lFalseUsed)
	{
	    result.addImplicitCompileResult (lFalse, false);
	}
	context.add (lExit);
	return result;
    }

    /** Compile an 'and' expression whose value is only used as a boolean */
    @SuppressWarnings ("unused")
    private void compileBooleanAnd (final TreeCompilerContext context, final LispList e)
    {
	// (define foo (a b) (if (and a b) 1 2))
	final LabelNode l1 = new LabelNode ();
	for (int i = 1; i < e.size (); i++)
	{
	    final CompileResultSet r = context.compile (e.get (i), true);
	    context.convert (r, boolean.class, false, true);
	    context.add (new JumpInsnNode (IFEQ, l1));
	}
	// True case
	final LabelNode l2 = new LabelNode ();
	context.add (new InsnNode (ICONST_1));
	context.add (new JumpInsnNode (GOTO, l2));

	// False case
	context.add (l1);
	context.add (new InsnNode (ICONST_0));

	// Jump here after true case or fall through after false.
	// Return final value.
	context.add (l2);
    }

    /** Compile an 'and' expression whose value will be ignored. */
    private void compile2void (final TreeCompilerContext context, final LispList e)
    {
	// (define foo (a b) (and) 1)
	// (define foo (a b) (and a b) 2)
	// (define boolean:foo (a b) (and) 1)
	// (define foo () (and (printf "foo") (printf "bar")) 4)
	if (e.size () > 0)
	{
	    final LabelNode l1 = new LabelNode ();
	    for (int i = 1; i < e.size (); i++)
	    {
		final CompileResultSet crs = context.compile (e.get (i), false);
		context.convert (crs, boolean.class, false, true);
		context.add (new JumpInsnNode (IFEQ, l1));
	    }
	    context.add (l1);
	}
    }

    // public InsnSegment compile (final TreeCompiler compiler, final Map<Symbol, LocalBinding>
    // locals, final LispList expression,
    // final boolean resultDesired)
    // {
    // final InsnSegment result = null;
    // final InsnSegment trueCase = null;
    // // Need to say that this segment ends in false, but it does not matter how it gets there
    // final InsnSegment falseCase = compiler.compile (locals, false, boolean.class, false, true);
    // final Label l1 = new Label ();
    // InsnSegment previous = result;
    // for (int i = 1; i < expression.size (); i++)
    // {
    // final InsnSegment seg = compiler.compile (locals, expression.get (i), boolean.class, false,
    // true);
    // previous.getTail ().addCase (null/* default */, seg);
    // seg.addCase (new Object[]
    // {"false", "s0"}, falseCase);
    // previous = seg;
    // }
    // // True case
    // previous.addCase (null, trueCase);
    // result.addTail (trueCase);
    // result.addTail (falseCase);
    // return result;
    // }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueClass == null)
	{
	    compile2void (generator, mv, expression);
	}
	else if (valueClass.equals (boolean.class))
	{
	    compileBooleanAnd (generator, mv, expression);
	}
	else
	{
	    compileAnd (generator, mv, expression, valueClass, allowNarrowing, liberalTruth);
	}
    }

    /** Compile an 'and' expression whose value will be ignored. */
    public void compile2void (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (a b) (and) 1)
	// (define foo (a b) (and a b) 2)
	if (e.size () > 0)
	{
	    final Label l1 = new Label ();
	    for (int i = 1; i < e.size (); i++)
	    {
		generator.compileExpression (mv, e.get (i), boolean.class, false, true);
		mv.visitJumpInsn (IFEQ, l1);
	    }
	    mv.visitLabel (l1);
	}
    }

    /** Compile an 'and' expression whose value is only used as a boolean */
    private void compileBooleanAnd (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (a b) (if (and a b) 1 2))
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    generator.compileExpression (mv, e.get (i), boolean.class, false, true);
	    mv.visitJumpInsn (IFEQ, l1);
	}
	// True case
	final Label l2 = new Label ();
	mv.visitInsn (ICONST_1);
	mv.visitJumpInsn (GOTO, l2);

	// False case
	mv.visitLabel (l1);
	mv.visitInsn (ICONST_0);

	// Jump here after true case or fall through after false.
	// Return final value.
	mv.visitLabel (l2);
    }

    private void compileAnd (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (a b) (and))
	// (define foo (a b) (and a b))
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	mv.visitInsn (ICONST_1);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	for (int i = 1; i < e.size (); i++)
	{
	    mv.visitInsn (POP);
	    generator.compileExpression (mv, e.get (i), Object.class, false, true);
	    mv.visitInsn (DUP);
	    final Label l3 = new Label ();
	    mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	    mv.visitJumpInsn (IFEQ, l3);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	    mv.visitJumpInsn (IFEQ, l1);
	    mv.visitLabel (l3);
	}
	// True case
	mv.visitJumpInsn (GOTO, l2);

	// False case
	mv.visitLabel (l1);
	mv.visitInsn (POP);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);

	// Jump here after true case or fall through after false.
	// Return final value.
	mv.visitLabel (l2);
	// (define int:foo (a b) (and a b))
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
