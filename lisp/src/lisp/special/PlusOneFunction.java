
package lisp.special;

import java.util.*;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.symbol.LispVisitor;

public class PlusOneFunction implements Opcodes, LispTreeWalker, LispTreeFunction
{
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	visitor.visitValue (expression.get (1));
	visitor.visitEnd (expression);
    }

    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResultSet rs = context.compile (expression.get (1), true);
	final List<CompileResult> result = new ArrayList<CompileResult> ();
	// null result case must be first
	for (final CompileResult compileResult : rs.getResults ())
	{
	    final List<org.objectweb.asm.tree.LabelNode> l0 = compileResult.getLabels ();
	    final LabelNode l1 = new LabelNode ();

	    if (compileResult instanceof ExplicitCompileResult)
	    {
		final Class<?> c = ((ExplicitCompileResult)compileResult).getResultClass ();
		if (c.equals (int.class))
		{
		    context.add (l0);
		    context.add (new InsnNode (ICONST_1));
		    context.add (new InsnNode (IADD));
		    result.add (new ExplicitCompileResult (l1, c));
		}
		else if (c.equals (double.class))
		{
		    context.add (l0);
		    context.add (new InsnNode (DCONST_1));
		    context.add (new InsnNode (DADD));
		    result.add (new ExplicitCompileResult (l1, c));
		}
		else // all other possible types
		{
		    throw new Error ("NYI");
		}
	    }
	    else if (compileResult instanceof ImplicitCompileResult)
	    {
		final Object x = ((ImplicitCompileResult)compileResult).getValue ();
		if (x instanceof Integer)
		{
		    // The result is an implied integer value. Use the same label and change the
		    // value
		    final int xx = (Integer)x;
		    result.add (new ImplicitCompileResult (l0, xx + 1));
		}
		else if (x instanceof Double)
		{// The result is an implied double value. Use the same label and change the value
		    final double xx = (Double)x;
		    result.add (new ImplicitCompileResult (l0, xx + 1));
		}
		else
		{
		    throw new Error ("NYI");
		}
	    }
	    else
	    {
		throw new Error ("NYI");
	    }
	}
	return new CompileResultSet (result);
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
