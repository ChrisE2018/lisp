/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

import java.util.List;

import lisp.eval.LexicalContext;
import lisp.lang.*;

public class StandardFunctionCell extends FunctionCell
{
    public StandardFunctionCell (final Symbol symbol)
    {
	super (symbol, false);
    }

    @Override
    public Object eval (final LexicalContext context, final List<? extends Object> form) throws Exception
    {
	final LispList actuals = new LispList ();
	for (int i = 1; i < form.size (); i++)
	{
	    actuals.add (context.eval (form.get (i)));
	}
	return apply (actuals);
    }
}
