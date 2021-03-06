
package lisp.special;

import java.util.List;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.symbol.LispVisitor;

public class WhenFunction implements LispCCFunction, Opcodes, LispTreeWalker, LispTreeFunction
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final List<?> expression)
    {
	visitor.visitStart (expression);
	visitor.visitBoolean (expression.get (1));
	for (int i = 2; i < expression.size () - 1; i++)
	{
	    visitor.visitIgnored (expression.get (i));
	}
	visitor.visitValue (expression.get (expression.size () - 1));
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo (boolean:x) (when x 3))
	// (define foo () (when 'x (printf "foo")))
	// (define foo () (when (not 'x) 3)))
	// At this point we can optimize handling of information returned from the compiler.compile
	// call. Any result that is not boolean can just be wired to goto l2.
	// Any result that is a constant true or false can go directly to l1 or l2.

	if (resultDesired)
	{
	    final CompileResults testResultSet = context.compile (expression.get (1), true);
	    final LabelNode lNull = new LabelNode ();// This label means we return null
	    context.convertIfFalse (testResultSet, false, true, lNull);
	    // Evaluate and discard body forms except the last one
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		final CompileResults r = context.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		context.convert (r, void.class, false, false);
	    }
	    final CompileResults result = context.compile (expression.last (), resultDesired);
	    // Changing null to false fixes the problem with collectPrimes

	    context.add (lNull);
	    final LabelNode ll = new LabelNode ();
	    context.add (new JumpInsnNode (GOTO, ll));
	    result.addImplicitCompileResult (ll, null);
	    return result;
	}
	else
	{
	    final CompileResults testResultSet = context.compile (expression.get (1), true);
	    final LabelNode lNull = new LabelNode ();// This label means we return null
	    context.convertIfFalse (testResultSet, false, true, lNull);
	    // Evaluate and discard all body forms
	    for (int i = 2; i < expression.size (); i++)
	    {
		final CompileResults r = context.compile (expression.get (i), false);
		// Do something with r to throw away garbage if required
		context.convert (r, void.class, false, false);
	    }
	    context.add (lNull);
	    return new CompileResults ();
	}
    }

    // CONSIDER Another idea(s): (1) attach instructions to the TreeCompilerContext (make it keep
    // straight where they go). (2) Add our own new "meta" instructions to do higher level
    // things like convert Object to int or boolean. Follow with another pass to make these
    // concrete.

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final List<?> e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (when x 1 2))
	// (define foo (x) (when x 1 (printf "a%n") (printf "b%n") 3))

	final Label l1 = new Label ();
	final Label l2 = new Label ();
	generator.compileExpression (mv, e.get (1), boolean.class, false, true);
	mv.visitJumpInsn (IFEQ, l2);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Don't pop the last value
	generator.compileExpression (mv, e.get (e.size () - 1), valueType, allowNarrowing, liberalTruth);
	mv.visitJumpInsn (GOTO, l1);

	// False case.
	mv.visitLabel (l2);
	generator.pushDefaultValue (mv, valueType, false);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l1);
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
