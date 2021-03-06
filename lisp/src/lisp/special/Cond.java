
package lisp.special;

import lisp.eval.*;
import lisp.lang.LispList;

public class Cond extends LogicDefiner
{
    @DefineLisp (special = true, classname = "lisp.special.CondFunction")
    public Object cond (final LexicalContext context, final Object... clauses) throws Exception
    {
	// (setq a 3)
	// (cond ((eq a 1) 'alpha) ((= a 2) 'beta) ((= a 3) 'gamma) (true 'delta))
	Object result = null;
	for (int i = 0; i < clauses.length; i++)
	{
	    final LispList clause = (LispList)clauses[i];
	    final Object key = clause.get (0);
	    final Object selected = context.eval (key);
	    if (isTrue (selected))
	    {
		result = selected;
		for (int j = 1; j < clause.size (); j++)
		{
		    result = context.eval (clause.get (j));
		}
		return result;
	    }
	}
	return result;
    }
}
