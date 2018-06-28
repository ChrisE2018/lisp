
package lisp.special;

import java.util.List;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.asm.instructions.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.lang.Symbol;
import lisp.symbol.LispVisitor;

public class QuoteFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final List<?> expression)
    {
	visitor.visitStart (expression);
	visitor.visitConstantValue (expression.get (1));
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	if (resultDesired)
	{
	    final Object value = expression.get (1);
	    final LabelNode ll = new LabelNode ();
	    context.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResults (new ImplicitResult (ll, value));
	}
	return null;
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final List<?> e, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo () (quote bar))
	final Object value = e.get (1);
	if (boolean.class.equals (valueClass))
	{
	    if (value instanceof Boolean)
	    {
		mv.visitLdcInsn (value);
	    }
	    else
	    {
		mv.visitLdcInsn (true);
	    }
	}
	else if (valueClass != null)
	{
	    final Symbol quote = (Symbol)e.get (0);
	    final Symbol reference = quote.gensym ();
	    final Object quoted = e.get (1);
	    final Class<?> quotedClass = quoted.getClass ();
	    generator.addQuotedConstant (reference, quoted);
	    final String typeDescriptor = Type.getType (quotedClass).getDescriptor ();
	    // LOGGER.finer (new LogString ("Quoted reference to %s (%s)", typeDescriptor, quoted));
	    mv.visitVarInsn (ALOAD, 0);
	    final String classInternalName = generator.getClassType ().getInternalName ();
	    mv.visitFieldInsn (GETFIELD, classInternalName, reference.getName (), typeDescriptor);
	    // generator.coerceRequiredXX (mv, valueType);
	    generator.convert (mv, quotedClass, valueClass, allowNarrowing, liberalTruth);
	}
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
