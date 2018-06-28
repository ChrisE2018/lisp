
package lisp.special;

import java.util.List;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.symbol.LispVisitor;

public class PrognFunction implements LispCCFunction, Opcodes, LispTreeWalker, LispTreeFunction
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final List<?> expression)
    {
	visitor.visitStart (expression);
	for (int i = 1; i < expression.size () - 1; i++)
	{
	    visitor.visitIgnored (expression.get (i));
	}
	visitor.visitValue (expression.get (expression.size () - 1));
	visitor.visitEnd (expression);
    }

    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	for (int i = 1; i < expression.size () - 1; i++)
	{
	    final CompileResults resultSet = context.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    context.convert (resultSet, void.class, false, false);
	}
	return context.compile (expression.last (), true);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList expr,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo () (progn (printf "a%n") (printf "b%n") 3))
	if (expr.size () == 0)
	{
	    generator.pushDefaultValue (mv, valueType, true);
	}
	else
	{
	    for (int i = 1; i < expr.size () - 1; i++)
	    {
		generator.compileExpression (mv, expr.get (i), null, false, false);
	    }
	    generator.compileExpression (mv, expr.last (), valueType, allowNarrowing, liberalTruth);
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
