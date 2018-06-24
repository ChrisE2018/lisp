
package lisp.eval;

public class LogicDefiner extends Definer
{
    /**
     * All functions that are conditional on an argument being true use this definition. null and
     * boolean false are considered false. Everything else is true.
     */
    protected boolean isTrue (final Object value)
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
}
