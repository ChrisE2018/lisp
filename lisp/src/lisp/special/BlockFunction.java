
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.cc4.*;
import lisp.lang.LispList;

public class BlockFunction implements Opcodes, LispTreeFunction
{
    @Override
    public CompileResultSet compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	for (int i = 1; i < expression.size () - 1; i++)
	{
	    final CompileResultSet resultSet = context.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    context.convert (resultSet, void.class, false, false);
	}
	return context.compile (expression.last (), true);
    }
}
