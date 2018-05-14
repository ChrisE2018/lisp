
package plan;

import java.util.*;

public class OpenGoalCondition
{
    private final Node node;
    private final Condition condition;
    private final List<Achiever> achievers = new ArrayList<Achiever> ();

    OpenGoalCondition (final Node node, final Condition condition)
    {
	this.node = node;
	this.condition = condition;
    }

    public Node getNode ()
    {
	return node;
    }

    public Condition getCondition ()
    {
	return condition;
    }

    public List<Achiever> getAchievers ()
    {
	return achievers;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (node.getName ());
	buffer.append (" ");
	buffer.append (condition);
	buffer.append (">");
	return buffer.toString ();
    }
}
