
package lisp;

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
