
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.asm.instructions.LabelNode;
import lisp.cc.BlockBinding;
import lisp.cc4.*;
import lisp.lang.*;

public class BlockNamedFunction implements Opcodes, LispTreeFunction
{
    @Override
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo () (block-named bar 78))
	// (define foo () (block-named foo (return-from foo 6) 9))
	// (define foo () (block-named foo (block-named bar (return-from foo 6) 7) 8)) => 6
	// (define foo () (block-named foo (block-named bar (return-from bar 6) 7) 8)) => 8
	final Symbol blockName = (Symbol)expression.get (1);
	final LabelNode l1 = new LabelNode ();
	final BlockBinding bb = new BlockBinding (blockName, Object.class, l1);
	final TreeCompilerContext innerContext = context.bindBlock (bb);

	for (int i = 2; i < expression.size () - 1; i++)
	{
	    final CompileResults resultSet = innerContext.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    innerContext.convert (resultSet, void.class, false, false);
	}
	final CompileResults rs = context.compile (expression.last (), true);
	rs.add (new ExplicitResult (l1, Object.class));
	return rs;
    }
}
