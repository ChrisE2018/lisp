
package plan;

import java.util.List;

public class EmptyAchiever extends Achiever
{
    private final Plan child;

    EmptyAchiever (final Plan child)
    {
	super (null);
	this.child = child;
    }

    @Override
    public double getIncrementCost ()
    {
	return 0.0;
    }

    @Override
    public Plan expand (final List<Plan> result)
    {
	throw new UnsupportedOperationException ("Can't expand EmptyAchiever");
    }

    @Override
    public Plan getChild ()
    {
	return child;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
