
package lisp;

public class LispBraceList extends LispList
{
    @Override
    public ListKind getListKind ()
    {
	return ListKind.BRACE;
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
