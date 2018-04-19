
package lisp;

public class LispBracketList extends LispList
{
    @Override
    public ListKind getListKind ()
    {
	return ListKind.BRACKET;
    }

    // public String toString ()
    // {
    // final StringBuilder buffer = new StringBuilder ();
    // buffer.append ("#<");
    // buffer.append (getClass ().getSimpleName ());
    // buffer.append (">");
    // return buffer.toString ();
    // }
}
