
package lisp;

import java.util.ArrayList;

public abstract class LispList extends ArrayList<Lisp> implements Lisp
{
    public abstract ListKind getListKind ();

    public void print (final StringBuilder buffer)
    {
	buffer.append (getListKind ().getOpenChar ());
	for (int i = 0; i < size (); i++)
	{
	    if (i > 0)
	    {
		buffer.append (' ');
	    }
	    get (i).print (buffer);
	}
	buffer.append (getListKind ().getCloseChar ());
    }

    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
