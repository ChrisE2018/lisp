
package plan;

public class Predicate
{
    /** The Predicate name. */
    private final String name;

    public Predicate (final String name)
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
