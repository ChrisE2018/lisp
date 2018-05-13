
package plan;

import java.util.List;

public class LinkAchiever extends Achiever
{
    private final Node achieverNode;
    private final Node protectedNode;
    private final Bindings bindings;

    LinkAchiever (final Plan parent, final Node achieverNode, final Node protectedNode)
    {
	super (parent);
	this.achieverNode = achieverNode;
	this.protectedNode = protectedNode;
	bindings = new Bindings ();
    }

    LinkAchiever (final Plan parent, final Node achieverNode, final Node protectedNode, final Bindings bindings)
    {
	super (parent);
	this.achieverNode = achieverNode;
	this.protectedNode = protectedNode;
	this.bindings = bindings;
    }

    @Override
    public double getIncrementCost ()
    {
	return 1.0;
    }

    public Node getAchieverNode ()
    {
	return achieverNode;
    }

    public Node getProtectedNode ()
    {
	return protectedNode;
    }

    public Bindings getBindings ()
    {
	return bindings;
    }

    @Override
    public Plan expand (final List<Plan> result)
    {
	throw new UnsupportedOperationException ("Can't expand LinkAchiever");
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
