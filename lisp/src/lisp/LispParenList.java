
package lisp;

import java.util.List;

public class LispParenList extends LispList
{
    public LispParenList ()
    {
	super ();
    }

    public LispParenList (final List<Lisp> members)
    {
	super (members);
    }

    @Override
    public ListKind getListKind ()
    {
	return ListKind.PAREN;
    }

    // @Override
    // public String toString ()
    // {
    // final StringBuilder buffer = new StringBuilder ();
    // buffer.append ("#<");
    // buffer.append (getClass ().getSimpleName ());
    // buffer.append (">");
    // return buffer.toString ();
    // }
}
