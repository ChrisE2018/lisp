
package lisp;

import java.util.List;

public class LispParenList extends LispList
{
    public LispParenList ()
    {
	super ();
    }

    public LispParenList (final List<Object> members)
    {
	super (members);
    }

    @Override
    public ListKind getListKind ()
    {
	return ListKind.PAREN;
    }
}
