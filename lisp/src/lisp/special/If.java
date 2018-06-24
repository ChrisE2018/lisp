
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class If extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "if", classname = "lisp.special.IfFunction")
    public Object ifEvaluator (final LexicalContext context, final Object test, final Object trueClause,
            final Object... elseExprs) throws Exception
    {
	if (isTrue (context.eval (test)))
	{
	    return context.eval (trueClause);
	}
	Object result = Boolean.TRUE;
	for (int i = 0; i < elseExprs.length; i++)
	{
	    final Object arg = elseExprs[i];
	    final Object value = context.eval (arg);
	    result = value;
	}
	return result;
    }
}
