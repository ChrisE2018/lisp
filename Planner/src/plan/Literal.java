
package plan;

import lisp.lang.LispList;

public class Literal
{
    private final LispList fact;

    public Literal (final LispList fact)
    {
	this.fact = fact;
    }

    public void print (final StringBuilder buffer)
    {
	fact.print (buffer);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }
}
