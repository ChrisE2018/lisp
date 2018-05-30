
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.cc.CompilerGenerator;
import lisp.eval.*;

public class Unless extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "unless")
    public Object unlessForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	if (!isTrue (context.eval (test)))
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

    @DefineLisp (special = true, name = "unless", compiler = true)
    public void compileUnless (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
