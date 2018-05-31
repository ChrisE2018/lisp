
package lisp.special;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.cc.CompilerGenerator;
import lisp.eval.*;

public class And extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true)
    public Object and (final LexicalContext context, final Object... arguments) throws Exception
    {
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = context.eval (arg);
	    if (!isTrue (value))
	    {
		return false;
	    }
	    result = value;
	}
	return result;
    }

    @DefineLisp (special = true, name = "and", compiler = true)
    public void compileGeneralAnd (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expression,
            final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueClass == null)
	{
	    compileVoidAnd (generator, mv, expression);
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
    private void compileVoidAnd (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e)
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
