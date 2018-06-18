
package lisp;

import java.util.List;

public class BracketList extends LispList
{
    public BracketList (final List<Object> p)
    {
	super (p);
    }

    public BracketList (final Object... p)
    {
	super (p);
    }

    /** Character that starts the list. */
    @Override
    public char getOpenChar ()
    {
	return '[';
    }

    /** Character that finishes the list. */
    @Override
    public char getCloseChar ()
    {
	return ']';
    }

    private BracketList requireBracketList (final Object o)
    {
	return (BracketList)o;
    }

    /** First element of a list, using standard Lisp terminology. */
    @Override
    public Object car ()
    {
	final Object result = get (0);
	if (result instanceof LispList)
	{
	    return requireBracketList (result);
	}
	return result;
    }

    /** First element of a list. */
    @Override
    public Object first ()
    {
	final Object result = get (0);
	if (result instanceof LispList)
	{
	    return requireBracketList (result);
	}
	return result;
    }

    @Override
    public Object last ()
    {
	final Object result = get (size () - 1);
	if (result instanceof LispList)
	{
	    return requireBracketList (result);
	}
	return result;
    }

    @Override
    public LispList subList (final int i)
    {
	final BracketList result = new BracketList ();
	result.addAll (subList (i, size ()));
	return result;
    }

    /** Print in the same format as the reader. */
    @Override
    public void print (final StringBuilder buffer)
    {
	buffer.append ('[');
	for (int i = 0; i < size (); i++)
	{
	    if (i > 0)
	    {
		buffer.append (',');
		buffer.append (' ');
	    }
	    final Object rawItem = get (i);
	    if (rawItem instanceof List)
	    {
		final List<?> item = (List<?>)get (i);
		for (int j = 0; j < item.size (); j++)
		{
		    if (j > 0)
		    {
			buffer.append (' ');
		    }
		    LispReader.printElement (buffer, item.get (j));
		}
	    }
	    else
	    {
		LispReader.printElement (buffer, rawItem);
	    }
	}
	buffer.append (']');
    }
}
