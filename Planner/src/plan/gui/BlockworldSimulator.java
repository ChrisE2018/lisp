
package plan.gui;

import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.List;
import java.util.Map.Entry;

import javax.swing.*;
import javax.swing.Timer;

import lisp.lang.Symbol;
import plan.*;
import util.FontUtil;

public class BlockworldSimulator extends JPanel implements ActionListener
{
    private static final long ACTION_INTERVAL = 2000;
    private Plan plan;

    private final List<Node> simulatedNodes = new ArrayList<Node> ();

    private long actionTimestamp;

    private final Set<Symbol> blocks = new HashSet<Symbol> ();

    private final Map<Symbol, Sprite> sprites = new HashMap<Symbol, Sprite> ();

    private final Timer timer = new Timer (1, this);

    /** Conditions that are currently true. */
    private final List<Condition> state = new ArrayList<Condition> ();

    /** Goals of the original plan. */
    private final List<Condition> goals = new ArrayList<> ();

    public BlockworldSimulator ()
    {
	timer.start ();
    }

    /** Execute a timer action. */
    @Override
    public void actionPerformed (final ActionEvent e)
    {
	final long now = System.currentTimeMillis ();
	if (actionTimestamp + ACTION_INTERVAL < now)
	{
	    // System.out.printf ("Perform next action %n");
	    final List<Node> selection = getFirstNodes ();
	    if (!selection.isEmpty ())
	    {
		final Node selected = selection.get (0);
		final StringBuilder buffer = new StringBuilder ();
		buffer.append ("Perform next action ");
		buffer.append (selected.getName ());
		buffer.append (" ");
		buffer.append (selected.getAction ());
		System.out.println (buffer);
		simulate (selected);
	    }
	    else
	    {
		System.out.printf ("Simulation complete %n");
		timer.stop ();
	    }
	    actionTimestamp = now;
	}
	for (final Sprite sprite : sprites.values ())
	{
	    final int dx = sprite.destination.x - sprite.x;
	    final int dy = sprite.destination.y - sprite.y;
	    final int dw = sprite.destination.width - sprite.width;
	    final int dh = sprite.destination.height - sprite.height;
	    final int sx = sign (dx);
	    final int sy = sign (dy);
	    final int sw = sign (dw);
	    final int sh = sign (dh);
	    sprite.x += sx;
	    sprite.y += sy;
	    sprite.width += sw;
	    sprite.height += sh;
	}
	repaint ();
	// Need to periodically advance through the plan and "execute" an action.
	// Need to determine current variable bindings.
	// Need a graphic for each action.
    }

    private int sign (final int dx)
    {
	if (dx > 0)
	{
	    return 1;
	}
	else if (dx < 0)
	{
	    return -1;
	}
	return 0;
    }

    /** Get all nodes with no predecessors. These will be placed on the left. */
    private List<Node> getFirstNodes ()
    {
	final List<Node> result = new ArrayList<Node> ();
	for (final Node node : plan.getNodes ())
	{
	    if (!simulatedNodes.contains (node))
	    {
		if (!hasNewPredecessor (node, simulatedNodes))
		{
		    result.add (node);
		}
	    }
	}
	return result;
    }

    private boolean hasNewPredecessor (final Node node, final Collection<Node> marked)
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

    private void simulate (final Node node)
    {
	System.out.printf ("Simulate %s %n", node);
	for (final Condition c : node.getDeleteConditions ())
	{
	    state.remove (c);
	}
	for (final Condition c : node.getAddConditions ())
	{
	    state.add (c);
	}
	System.out.printf ("State %s %n", state);
	final StringBuilder buffer = new StringBuilder ();
	final Map<Symbol, Symbol> constraints = plan.getConstraints ();
	if (!constraints.isEmpty ())
	{
	    for (final Entry<Symbol, Symbol> entry : constraints.entrySet ())
	    {
		buffer.append (entry.getKey ());
		buffer.append (" != ");
		buffer.append (entry.getValue ());
		buffer.append (" ");
	    }
	    System.out.printf ("Constraints %s %n", buffer);
	}
	update ();
	simulatedNodes.add (node);
    }

    private void update ()
    {
	final int w = Math.max (100, getWidth ()) / blocks.size ();
	final int size = Math.max (50, Math.min (getWidth (), getHeight ()) / 10);
	final int height3 = getHeight () / 3;
	final int height = size;
	final int baseline = height3 * 2;
	int x = size / 10;
	final int y = Math.max (10, baseline - height + 10);
	final Set<Symbol> positionedBlocks = new HashSet<Symbol> ();
	final int limit = state.size ();
	for (int i = 0; positionedBlocks.size () < blocks.size () && i < limit; i++)
	{
	    for (final Condition c : state)
	    {
		final Symbol p = c.getPredicate ();
		if (p.is ("ontable"))
		{
		    final Symbol s = c.getTerms ().get (0);
		    if (!positionedBlocks.contains (s))
		    {
			final Sprite sprite = sprites.get (s);
			if (sprite == null)
			{
			    System.out.printf ("No sprite for %s %n", s);
			}
			else
			{
			    sprite.destination.x = x;
			    sprite.destination.y = y;
			    sprite.destination.width = size;
			    sprite.destination.height = height;
			    positionedBlocks.add (s);
			    System.out.printf ("Table block %s at %s %n", s, x);
			    x += w;
			}
		    }
		}
		else if (p.is ("on"))
		{
		    final Symbol upper = c.getTerms ().get (0);
		    final Symbol lower = c.getTerms ().get (1);
		    if (!positionedBlocks.contains (upper))
		    {
			if (positionedBlocks.contains (lower))
			{
			    final Sprite upperSprite = sprites.get (upper);
			    final Sprite lowerSprite = sprites.get (lower);
			    System.out.printf ("Simulate %s on %s %n", upper, lower);
			    upperSprite.destination.x = lowerSprite.destination.x;
			    upperSprite.destination.y = lowerSprite.destination.y - height;
			    upperSprite.destination.width = size;
			    upperSprite.destination.height = height;
			    positionedBlocks.add (upper);
			}
		    }
		}
		else if (p.is ("color"))
		{
		    try
		    {
			final Symbol block = c.getTerms ().get (0);
			final Sprite sprite = sprites.get (block);
			if (sprite != null)
			{
			    final Color color = getColor (c.getTerms ().get (1).getName ());
			    sprite.setBackColor (color);
			}
		    }
		    catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e)
		    {
		    }
		}
	    }
	}
    }

    private Color getColor (final String colorName)
            throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException
    {
	final Field field = Color.class.getField (colorName);
	if (field == null)
	{
	    return null;
	}
	else
	{
	    return (Color)field.get (null);
	}
    }

    // private Set<Symbol> getLiterals (final Node node)
    // {
    // final Set<Symbol> result = new HashSet<> ();
    // for (final Condition c : node.getAddConditions ())
    // {
    // for (final Symbol s : c.getTerms ())
    // {
    // if (!s.isVariable ())
    // {
    // result.add (s);
    // }
    // }
    // }
    // return result;
    // }

    @Override
    public void paintComponent (final Graphics g)
    {
	final int height3 = getHeight () / 3;
	final int baseline = height3 * 2;
	g.setColor (Color.cyan);
	g.fillRect (0, 0, getWidth (), baseline);
	g.setColor (Color.green);
	g.fillRect (0, baseline, getWidth (), height3);
	g.setColor (Color.YELLOW);
	g.fillOval (100, 100, 50, 50);
	for (final Sprite sprite : sprites.values ())
	{
	    sprite.paint (g);
	}
	g.setFont (FontUtil.getFont (g, 12));
	g.setColor (Color.black);
	for (int i = 0; i < goals.size (); i++)
	{
	    final Condition goal = goals.get (i);
	    g.drawString (goal.toString (), 10, 20 + i * 15);
	}
    }

    private void setPlan (final Plan plan)
    {
	System.out.printf ("setPlan %s%n", plan);
	if (this.plan != plan)
	{
	    this.plan = plan;
	    blocks.clear ();
	    sprites.clear ();
	    simulatedNodes.clear ();
	    final Node initialNode = plan.getInitialNode ();
	    for (final Condition c : initialNode.getAddConditions ())
	    {
		for (final Symbol s : c.getTerms ())
		{
		    blocks.add (s);
		}
	    }
	    for (final Symbol s : blocks)
	    {
		final Sprite sprite = new Sprite (s, 0, 0, 10, 10, s.getName ());
		sprites.put (s, sprite);
	    }
	    final List<Node> goalNodes = plan.getOriginalGoalNodes ();
	    goals.clear ();
	    for (final Node node : goalNodes)
	    {
		goals.addAll (node.getGoalConditions ());
	    }
	    actionTimestamp = System.currentTimeMillis ();
	    // System.out.printf ("new setPlan %s%n", plan);
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (plan);
	buffer.append (">");
	return buffer.toString ();
    }

    private static Map<Plan, JFrame> simulationViews = new HashMap<Plan, JFrame> ();

    public static void makeView (final Plan plan)
    {
	if (plan == null)
	{
	    System.out.printf ("Can't view null plan %n");
	    return;
	}
	JFrame frame = simulationViews.get (plan);
	if (frame == null)
	{
	    frame = new JFrame ("" + plan.getName () + " Simulation");
	    frame.setDefaultCloseOperation (JFrame.HIDE_ON_CLOSE);
	    final BlockworldSimulator simulatorView = new BlockworldSimulator ();
	    frame.setSize (900, 800);
	    // frame.pack ();
	    frame.getContentPane ().add (simulatorView);
	    simulatorView.setPlan (plan);
	    simulationViews.put (plan, frame);
	}
	final JFrame f = frame;
	java.awt.EventQueue.invokeLater (new Runnable ()
	{
	    @Override
	    public void run ()
	    {
		f.setVisible (true);
		f.toFront ();
	    }
	});
    }
}
