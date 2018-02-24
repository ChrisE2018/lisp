
package lisp;

public class LispParenList extends LispList
{
    @Override
    public ListKind getListKind ()
    {
	return ListKind.PAREN;
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
