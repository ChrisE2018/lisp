
package plan.gui;

import java.awt.*;
import java.util.*;
import java.util.List;

import plan.*;

/** Layout a partially ordered plan geometrically. */
public class PlanLayout
{
    private final int X_GAP = 50;
    private final int Y_GAP = 50;

    public List<Sprite> getLayout (final Plan plan, final Rectangle r)
    {
	final List<Sprite> result = new ArrayList<Sprite> ();
	final Set<Node> marked = new HashSet<Node> ();
	final List<List<Node>> columns = new ArrayList<List<Node>> ();
	int maxColumnSize = 0;
	while (marked.size () < plan.getNodes ().size ())
	{
	    final List<Node> column = getFirstNodes (plan, marked);
	    marked.addAll (column);
	    columns.add (column);
	    final int columnSize = column.size ();
	    if (columnSize > maxColumnSize)
	    {
		maxColumnSize = columnSize;
	    }
	    // System.out.printf ("Column %s %n", column);
	}
	final int columnWidth = r.width / columns.size ();
	final int rowHeight = r.height / maxColumnSize;
	int x = X_GAP / 2;
	for (final List<Node> column : columns)
	{
	    int y = Y_GAP / 2;
	    y += (r.height - column.size () * rowHeight) / 2;
	    for (final Node node : column)
	    {
		final Sprite sprite = makeSprite (node);
		sprite.x = x;
		sprite.y = y;
		sprite.width = columnWidth - X_GAP;
		sprite.height = rowHeight - Y_GAP;
		result.add (sprite);
		y += rowHeight;
	    }
	    x += columnWidth;
	}

	final Sprite info = new Sprite (plan, "Name: " + plan.getName ());
	final Plan parent = plan.getParent ();
	info.addLabel ("Parent: " + (parent == null ? "None" : parent.getName ()));
	final Object revisionGoal = plan.getRevisionGoal ();
	if (revisionGoal != null)
	{
	    info.addLabel ("Expanded Goal: " + revisionGoal);
	}
	// final Object revisionSupport = plan.getRevisionSupport ();
	// if (revisionSupport != null)
	// {
	// info.addLabel ("Revision Support: " + revisionSupport);
	// }
	final double b = plan.getIncrementCost ();
	final double c = plan.getCost ();
	if (parent == null)
	{
	    info.addLabel ("Act: %.1f = [root]%.1f + %.1f[incr]", c, 0.0, b);
	}
	else
	{
	    final double a = parent.getCost ();
	    info.addLabel ("Act: %.1f = [%s]%.1f + %.1f[incr]", c, parent.getName (), a, b);
	}
	final double e = plan.estimateRemainingCost ();
	final double t = c + e;
	info.addLabel ("Tot: %.1f = [act]%.1f + %.1f[est]", t, c, e);

	info.x = 0;
	info.y = 0;
	info.width = 300;
	info.height = 90;
	result.add (info);
	return result;
    }

    private Sprite makeSprite (final Node node)
    {
	final Sprite result = new Sprite (node, node.getName ().getName ());
	final Action action = node.getAction ();
	if (action != null)
	{
	    result.addLabel ("Action: " + action.getName ());
	}
	addLabels (result, "-", node.getDeleteConditions ());
	addLabels (result, "+", node.getAddConditions ());
	for (final ProtectionInterval pi : node.getCausalLinks ())
	{
	    result.addLabel ("PI: " + pi.getCondition ().toString () + "=>" + pi.getProtectedNode ().getName ());
	}
	addLabels (result, "?", Color.red, node.getGoalConditions ());
	for (final ProtectionInterval pi : node.getProtectedGoals ())
	{
	    result.addLabel (Color.green, "PI: " + pi.getCondition ().toString ());
	}

	return result;
    }

    private void addLabels (final Sprite result, final String key, final List<Condition> conditions)
    {
	for (final Condition c : conditions)
	{
	    result.addLabel (key + ": " + c.toString ());
	}
    }

    private void addLabels (final Sprite result, final String key, final Color color, final List<Condition> conditions)
    {
	for (final Condition c : conditions)
	{
	    result.addLabel (color, key + ": " + c.toString ());
	}
    }

    /** Get all nodes with no predecessors. These will be placed on the left. */
    private List<Node> getFirstNodes (final Plan plan, final Set<Node> marked)
    {
	final List<Node> result = new ArrayList<Node> ();
	for (final Node node : plan.getNodes ())
	{
	    if (!marked.contains (node))
	    {
		if (!hasNewPredecessor (node, marked))
		{
		    result.add (node);
		}
	    }
	}
	return result;
    }

    private boolean hasNewPredecessor (final Node node, final Set<Node> marked)
    {
	for (final Node p : node.getPrevious ())
	{
	    if (!marked.contains (p))
	    {
		return true;
	    }
	}
	return false;
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
