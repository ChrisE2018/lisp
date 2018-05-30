
package lisp.eval;

public class ControlPrimitives extends Definer
{
    // /**
    // * All functions that are conditional on an argument being true use this definition. null and
    // * boolean false are considered false. Everything else is true.
    // */
    // private boolean isTrue (final Object value)
    // {
    // if (value != null)
    // {
    // if (value instanceof Boolean)
    // {
    // if (false == (Boolean)value)
    // {
    // return false;
    // }
    // }
    // return true;
    // }
    // return false;
    // }

    // @DefineLisp (special = true)
    // public Object or (final LexicalContext context, final Object... arguments) throws Exception
    // {
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // final Object value = context.eval (arg);
    // if (isTrue (value))
    // {
    // return value;
    // }
    // }
    // return false;
    // }

    // @DefineLisp (special = true)
    // public Object and (final LexicalContext context, final Object... arguments) throws Exception
    // {
    // Object result = Boolean.TRUE;
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // final Object value = context.eval (arg);
    // if (!isTrue (value))
    // {
    // return false;
    // }
    // result = value;
    // }
    // return result;
    // }

    // @DefineLisp (special = true, name = "if")
    // public Object ifEvaluator (final LexicalContext context, final Object test, final Object
    // trueClause,
    // final Object... elseExprs) throws Exception
    // {
    // if (isTrue (context.eval (test)))
    // {
    // return context.eval (trueClause);
    // }
    // Object result = Boolean.TRUE;
    // for (int i = 0; i < elseExprs.length; i++)
    // {
    // final Object arg = elseExprs[i];
    // final Object value = context.eval (arg);
    // result = value;
    // }
    // return result;
    // }

    // @DefineLisp (special = true, name = "unless")
    // public Object unlessForm (final LexicalContext context, final Object test, final Object...
    // arguments) throws Exception
    // {
    // if (!isTrue (context.eval (test)))
    // {
    // Object result = true;
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // result = context.eval (arg);
    // }
    // return result;
    // }
    // return false;
    // }

    // @DefineLisp (special = true, name = "while")
    // public Object whileForm (final LexicalContext context, final Object test, final Object...
    // arguments) throws Exception
    // {
    // Object result = true;
    // while (isTrue (context.eval (test)))
    // {
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // result = context.eval (arg);
    // }
    // }
    // return result;
    // }

    // @DefineLisp (special = true, name = "until")
    // public Object untilForm (final LexicalContext context, final Object test, final Object...
    // arguments) throws Exception
    // {
    // Object result = true;
    // while (!isTrue (context.eval (test)))
    // {
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // result = context.eval (arg);
    // }
    // }
    // return result;
    // }

    // @DefineLisp (special = true, name = "repeat")
    // public Object repeat (final LexicalContext context, final Object count, final Object...
    // arguments) throws Exception
    // {
    // Object result = true;
    // final int n = (Integer)context.eval (count);
    // for (int j = 0; j < n; j++)
    // {
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // result = context.eval (arg);
    // }
    // }
    // return result;
    // }

    // @DefineLisp (special = true, name = "dotimes")
    // public Object dotimes (final LexicalContext context, final List<?> control, final Object...
    // arguments) throws Exception
    // {
    // Object result = true;
    // final Symbol var = (Symbol)control.get (0);
    // final int n = (Integer)context.eval (control.get (1));
    // final LexicalContext subcontext = new LexicalContext (context);
    // subcontext.bind (var, 0);
    // for (int j = 0; j < n; j++)
    // {
    // subcontext.set (var, j);
    // for (int i = 0; i < arguments.length; i++)
    // {
    // final Object arg = arguments[i];
    // result = subcontext.eval (arg);
    // }
    // }
    // return result;
    // }

    // @DefineLisp (special = true)
    // public Object cond (final LexicalContext context, final Object... clauses) throws Exception
    // {
    // // (setq a 3)
    // // (cond ((eq a 1) 'alpha) ((= a 2) 'beta) ((= a 3) 'gamma) (true 'delta))
    // Object result = null;
    // for (int i = 0; i < clauses.length; i++)
    // {
    // final LispList clause = (LispList)clauses[i];
    // final Object key = clause.get (0);
    // final Object selected = context.eval (key);
    // if (isTrue (selected))
    // {
    // result = selected;
    // for (int j = 1; j < clause.size (); j++)
    // {
    // result = context.eval (clause.get (j));
    // }
    // return result;
    // }
    // }
    // return result;
    // }

    @DefineLisp
    public Object sleep (final Object a) throws InterruptedException
    {
	final Object result = null;
	long ms = 0;
	if (a instanceof Integer)
	{
	    ms = 1000 * ((int)a);
	}
	else if (a instanceof Double)
	{
	    ms = Math.round (1000 * (double)a);
	}
	else
	{
	    throw new IllegalArgumentException ("Sleep seconds required " + a);
	}
	Thread.sleep (ms);
	return result;
    }

    @DefineLisp
    public void exit (final int status)
    {
	System.exit (status);
    }

    @DefineLisp
    public void exit ()
    {
	System.exit (0);
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
