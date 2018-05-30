
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.cc.CompilerGenerator;
import lisp.eval.*;

public class Or extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true)
    public Object or (final LexicalContext context, final Object... arguments) throws Exception
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = context.eval (arg);
	    if (isTrue (value))
	    {
		return value;
	    }
	}
	return false;
    }

    @DefineLisp (special = true, name = "or", compiler = true)
    public void compileGeneralOr (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueType == null)
	{
	    compileVoidOr (generator, mv, expression);
	}
	else if (valueType.equals (boolean.class))
	{
	    compileBooleanOr (generator, mv, expression);
	}
	else
	{
	    compileOr (generator, mv, expression, valueType, allowNarrowing, liberalTruth);
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
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
	    generator.compileExpression (mv, e.get (i), Object.class /* TODO */, false, true);
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
	generator.coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
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
