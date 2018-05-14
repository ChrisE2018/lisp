
package plan;

import java.util.List;

public class DeconflictAchiever extends Achiever
{
    private final Node protectedNode;
    private final Node achieverNode;

    DeconflictAchiever (final Plan parent, final Node protectedNode, final Node achieverNode)
    {
	super (parent);
	this.achieverNode = achieverNode;
	this.protectedNode = protectedNode;
    }

    @Override
    public double getIncrementCost ()
    {
	return 1.0;
    }

    public Node getProtectedNode ()
    {
	return protectedNode;
    }

    public Node getAchieverNode ()
    {
	return achieverNode;
    }

    @Override
    public ProtectionInterval expand ()
    {
	throw new UnsupportedOperationException ("Can't expand DeconflictAchiever");
    }

    @Override
    public Plan expand (final List<Plan> result)
    {
	throw new UnsupportedOperationException ("Can't expand DeconflictAchiever");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (achieverNode.getName ());
	buffer.append ("=>");
	buffer.append (protectedNode.getName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
