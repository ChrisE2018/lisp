
package plan;

import lisp.*;

public class Literal implements Lisp
{
    private final LispList fact;

    public Literal (final LispList fact)
    {
	this.fact = fact;
    }

    @Override
    public void print (final StringBuilder buffer)
    {
	fact.print (buffer);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	fact.print (buffer);
	buffer.append (">");
	return buffer.toString ();
    }
}
