
package lisp.lang;

import java.util.List;

/** Alternate implementation of List for braces. (not used) */
public class BraceList extends LispList
{
    public BraceList (final List<Object> p)
    {
	super (p);
    }

    public BraceList (final Object... p)
    {
	super (p);
    }

    /** Character that starts the list. */
    @Override
    public char getOpenChar ()
    {
	return '{';
    }

    /** Character that finishes the list. */
    @Override
    public char getCloseChar ()
    {
	return '}';
    }

    /** Convenience method that allows the caller to avoid a cast. */
    @Override
    public LispList getSublist (final int i)
    {
	final Object result = get (i);
	try
	{
	    return (LispList)result;
	}
	catch (final ClassCastException e)
	{
	    throw new IllegalArgumentException ("List member " + i + " is not a sublist", e);
	}
    }

    /** First element of a list, using standard Lisp terminology. */
    @Override
    public Object car ()
    {
	return get (0);
    }

    /** First element of a list. */
    @Override
    public Object first ()
    {
	return get (0);
    }

    @Override
    public Object last ()
    {
	return get (size () - 1);
    }

    @Override
    public LispList subList (final int i)
    {
	final LispList result = new LispList ();
	result.addAll (subList (i, size ()));
	return result;
    }

    /** Print in the same format as the reader. */
    @Override
    public void print (final StringBuilder buffer)
    {
	buffer.append ('{');
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
	buffer.append ('}');
    }
}
