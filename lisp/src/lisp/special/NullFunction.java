
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.*;
import lisp.cc4.*;
import lisp.lang.LispList;

public class NullFunction implements Opcodes, LispTreeFunction
{
    /**
     * Compile null test into primitive bytecode. This complex implementation is designed to produce
     * as little code as possible by deeply analyzing the cases. Implicit and Explicit results are
     * merged as much as possible.
     */
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	final CompileResults testResultSet = context.compile (expression.get (1), true);
	final LabelNode lFalse = new LabelNode (); // This label means we return false
	final LabelNode lTrue = new LabelNode (); // This label means we return true

	{
	    boolean hasObjectResult = false;
	    for (final CompileResult r : testResultSet.getResults ())
	    {
		if (r instanceof ExplicitResult)
		{
		    final ExplicitResult er = (ExplicitResult)r;
		    final Class<?> claz = er.getResultClass ();
		    if (!claz.isPrimitive ())
		    {
			context.add (er.getLabels ());
			hasObjectResult = true;
		    }
		    else
		    {
			throw new Error ("null cannot be applied to primitive types");
		    }
		}
	    }
	    if (hasObjectResult)
	    {
		context.add (new JumpInsnNode (IFNULL, lTrue));
		context.add (new JumpInsnNode (GOTO, lFalse));
	    }
	}
	{
	    boolean hasImplicitTrue = false;
	    for (final CompileResult r : testResultSet.getResults ())
	    {
		if (r instanceof ImplicitResult)
		{
		    final ImplicitResult ir = (ImplicitResult)r;
		    if (ir.getValue () == null)
		    {
			hasImplicitTrue = true;
			context.add (ir.getLabels ());
		    }
		}
	    }
	    if (hasImplicitTrue)
	    {
		context.add (new JumpInsnNode (GOTO, lTrue));
	    }
	}
	{
	    boolean hasImplicitFalse = false;
	    for (final CompileResult r : testResultSet.getResults ())
	    {
		if (r instanceof ImplicitResult)
		{
		    final ImplicitResult ir = (ImplicitResult)r;
		    if (ir.getValue () != null)
		    {
			hasImplicitFalse = true;
			context.add (ir.getLabels ());
		    }
		}
	    }
	    if (hasImplicitFalse)
	    {
		context.add (new JumpInsnNode (GOTO, lFalse));
	    }
	}
	final CompileResults result = new CompileResults ();
	result.addImplicitCompileResult (lFalse, false);
	result.addImplicitCompileResult (lTrue, true);
	return result;
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
