
package lisp;

public class LispBracketList extends LispList
{
    @Override
    public ListKind getListKind ()
    {
	return ListKind.BRACKET;
    }
}
