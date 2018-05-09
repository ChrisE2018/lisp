
package plan;

public class Term
{
    /** The variable name. */
    private final String name;

    public Term (final String name)
    {
	this.name = name;
    }

    /** The variable name. */
    public String getName ()
    {
	return name;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	buffer.append (">");
	return buffer.toString ();
    }
}
