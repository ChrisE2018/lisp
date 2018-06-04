
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.tree.*;

import lisp.LispList;
import lisp.cc.*;
import lisp.cc4.*;
import lisp.symbol.*;

public class WhenFunction implements LispCCFunction, Opcodes, LispTreeWalker, LispTreeFunction
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
    public Class<?> compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final Class<?> testClass = context.compile (expression.get (1), true);
	// At this point we can optimize handling of information returned from the compiler.compile
	// call. Any result that is not boolean can just be wired to goto l2.
	// Any result that is a constant true or false can go directly to l1 or l2.
	context.convert (testClass, boolean.class, false, true);

	final LabelNode l1 = new LabelNode ();// This label means we return false
	final LabelNode l2 = new LabelNode ();
	context.add (new JumpInsnNode (IFEQ, l1));

	for (int i = 2; i < expression.size () - 1; i++)
	{
	    final Class<?> r = context.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    context.convert (r, void.class, false, false);
	}
	final Class<?> result = context.compile (expression.last (), true);
	context.add (new JumpInsnNode (GOTO, l2));
	context.add (l1);
	if (resultDesired)
	{
	    context.add (new LdcInsnNode (false));
	}
	context.add (l2);
	return result;
    }

    // @Override
    // public Class<?> compile (final TreeCompiler compiler, final InsnList il, final Map<Symbol,
    // LocalBinding> locals,
    // final LispList expression, final boolean resultDesired)
    // {
    // final Class<?> testClass = compiler.compile (il, locals, expression.get (1), true);
    // // At this point we can optimize handling of information returned from the compiler.compile
    // // call. Any result that is not boolean can just be wired to goto l2.
    // // Any result that is a constant true or false can go directly to l1 or l2.
    // compiler.convert (il, testClass, boolean.class, false, true);
    //
    // final LabelNode l1 = new LabelNode ();// This label means we return false
    // final LabelNode l2 = new LabelNode ();
    // il.add (new JumpInsnNode (IFEQ, l1));
    //
    // for (int i = 2; i < expression.size () - 1; i++)
    // {
    // final Class<?> r = compiler.compile (il, locals, expression.get (i), false);
    // // Do something with r to throw away garbage if required
    // compiler.convert (il, r, void.class, false, false);
    // }
    // final Class<?> result = compiler.compile (il, locals, expression.last (), true);
    // il.add (new JumpInsnNode (GOTO, l2));
    // // This label means we return the result of the last nested form. The caller should place
    // // these labels in the best location and try to keep the Boolean.FALSE value implicit.
    // // final Object[][] resultx =
    // // {
    // // {l1, boolean.class, Boolean.FALSE},
    // // {l2, result}};
    // // return resultx;
    // // [TODO] Another idea(s): (1) attach instructions to the TreeCompilerContext (make it keep
    // // straight where they go). (2) Add our own new "meta" instructions to do higher level
    // // things like convert Object to int or boolean. Follow with another pass to make these
    // // concrete.
    // il.add (l1);
    // if (resultDesired)
    // {
    // il.add (new LdcInsnNode (false));
    // }
    // il.add (l2);
    // return result;
    // }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
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
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
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
