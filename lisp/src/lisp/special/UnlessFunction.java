
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.symbol.LispVisitor;

public class UnlessFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	visitor.visitBoolean (expression.get (1));
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
	// (define foo (boolean:x) (unless x 3))
	if (resultDesired)
	{
	    final CompileResultSet testResultSet = context.compile (expression.get (1), true);
	    // At this point we can optimize handling of information returned from the
	    // compiler.compile
	    // call. Any result that is not boolean can just be wired to goto l2.
	    // Any result that is a constant true or false can go directly to l1 or l2.

	    final LabelNode lNull = new LabelNode (); // This label means we return null
	    context.convertIfTrue (testResultSet, false, true, lNull);

	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		final CompileResultSet r = context.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		context.convert (r, void.class, false, false);
	    }
	    final CompileResultSet result = context.compile (expression.last (), true);
	    context.add (lNull);
	    final LabelNode ll = new LabelNode ();
	    result.addImplicitCompileResult (ll, null);
	    context.add (new JumpInsnNode (GOTO, ll));
	    return result;
	}
	else
	{
	    final CompileResultSet testResultSet = context.compile (expression.get (1), true);
	    // At this point we can optimize handling of information returned from the
	    // compiler.compile
	    // call. Any result that is not boolean can just be wired to goto l2.
	    // Any result that is a constant true or false can go directly to l1 or l2.

	    final LabelNode lNull = new LabelNode ();// This label means we return null
	    context.convertIfTrue (testResultSet, false, true, lNull);

	    for (int i = 2; i < expression.size (); i++)
	    {
		final CompileResultSet r = context.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		context.convert (r, void.class, false, false);
	    }
	    context.add (lNull);
	    return new CompileResultSet ();
	}
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (unless x 1 2))
	// (define foo (x) (unless x 1 (printf "a%n") (printf "b%n") 3))
	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Don't pop the last value
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	final Label l3 = new Label ();
	mv.visitJumpInsn (GOTO, l3);

	// False case where we must pop a value
	mv.visitLabel (l1);
	generator.pushDefaultValue (mv, valueType, true);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l3);
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
