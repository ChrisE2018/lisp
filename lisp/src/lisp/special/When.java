
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.cc.CompilerGenerator;
import lisp.eval.*;

public class When extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "when")
    public Object whenForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	if (isTrue (context.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = context.eval (arg);
	    }
	    return result;
	}
	return false;
    }

    @DefineLisp (special = true, name = "when", compiler = true)
    public void compileWhen (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
