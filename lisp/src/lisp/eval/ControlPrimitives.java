
package lisp.eval;

public class ControlPrimitives extends Definer
{
    /**
     * All functions that are conditional on an argument being true use this definition. null and
     * boolean false are considered false. Everything else is true.
     */
    private boolean isTrue (final Object value)
    {
	if (value != null)
	{
	    if (value instanceof Boolean)
	    {
		if (false == (Boolean)value)
		{
		    return false;
		}
	    }
	    return true;
	}
	return false;
    }

    @DefineLisp (special = true)
    public Object or (final Interpreter interpreter, final Object... arguments) throws Exception
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = interpreter.eval (arg);
	    if (isTrue (value))
	    {
		return value;
	    }
	}
	return false;
    }

    @DefineLisp (special = true)
    public Object and (final Interpreter interpreter, final Object... arguments) throws Exception
    {
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = interpreter.eval (arg);
	    if (!isTrue (value))
	    {
		return false;
	    }
	    result = value;
	}
	return result;
    }

    @DefineLisp (special = true, name = "if")
    public Object ifEvaluator (final Interpreter interpreter, final Object test, final Object trueClause,
            final Object... arguments) throws Exception
    {
	if (isTrue (interpreter.eval (test)))
	{
	    return interpreter.eval (trueClause);
	}
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = interpreter.eval (arg);
	    result = value;
	}
	return result;
    }

    @DefineLisp (special = true, name = "when")
    public Object whenForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	if (isTrue (interpreter.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	    return result;
	}
	return false;
    }

    @DefineLisp (special = true, name = "unless")
    public Object unlessForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	if (!isTrue (interpreter.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	    return result;
	}
	return false;
    }

    @DefineLisp (special = true, name = "while")
    public Object whileForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	Object result = true;
	while (isTrue (interpreter.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	}
	return result;
    }

    @DefineLisp (special = true, name = "until")
    public Object untilForm (final Interpreter interpreter, final Object test, final Object... arguments) throws Exception
    {
	Object result = true;
	while (!isTrue (interpreter.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = interpreter.eval (arg);
	    }
	}
	return result;
    }

    @DefineLisp (special = true)
    public Object progn (final Interpreter interpreter, final Object... arguments) throws Exception
    {
	Object result = null;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    result = interpreter.eval (arg);
	}
	return result;
    }

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
	buffer.append (">");
	return buffer.toString ();
    }
}
