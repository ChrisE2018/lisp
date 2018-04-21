
package lisp;

public class LispBraceList extends LispList
{
    @Override
    public ListKind getListKind ()
    {
	return ListKind.BRACE;
    }
}
