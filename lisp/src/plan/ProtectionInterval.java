
package plan;

import java.util.List;

public class ProtectionInterval
{
    /** The protected condition. */
    private final List<?> condition;

    /** The plan node that achieves this condition. */
    private final Node achiever;

    /**
     * The plan node that requires this condition. No node that denies the condition can be allowed
     * in the plan between the achiever and protectedNode
     */
    private final Node protectedNode;

    public ProtectionInterval (final List<?> condition, final Node achiever, final Node protectedNode)
    {
	this.condition = condition;
	this.achiever = achiever;
	this.protectedNode = protectedNode;
    }

    public List<?> getCondition ()
    {
	return condition;
    }

    public Node getAchiever ()
    {
	return achiever;
    }

    public Node getProtectedNode ()
    {
	return protectedNode;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (condition);
	buffer.append (" from: ");
	buffer.append (achiever.getName ());
	buffer.append (" to: ");
	buffer.append (protectedNode.getName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
