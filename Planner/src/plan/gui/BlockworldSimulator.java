
package plan.gui;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.Timer;

import lisp.Symbol;
import plan.*;

public class BlockworldSimulator extends JPanel implements ComponentListener, ActionListener
{
    private static final long ACTION_INTERVAL = 1000;
    private Plan plan;

    private final List<Node> simulatedNodes = new ArrayList<Node> ();

    private long actionTimestamp;

    private final Set<Symbol> blocks = new HashSet<Symbol> ();

    private final Map<Symbol, Sprite> sprites = new HashMap<Symbol, Sprite> ();

    private final Timer timer = new Timer (1, this);

    public BlockworldSimulator ()
    {
	// addComponentListener (this);
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
		simulate (selected);
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
	update (node);
	simulatedNodes.add (node);
    }

    private void update (final Node node)
    {
	final int w = Math.max (100, getWidth ()) / blocks.size ();
	final int size = Math.max (50, Math.min (getWidth (), getHeight ()) / 10);
	final int height3 = getHeight () / 3;
	final int height = size;
	final int baseline = height3 * 2;
	int x = size / 10;
	final int y = Math.max (10, baseline - height + 10);

	for (final Condition c : node.getAddConditions ())
	{
	    final Symbol p = c.getPredicate ();
	    if (p.is ("ontable"))
	    {
		for (final Symbol s : c.getTerms ())
		{
		    final Sprite sprite = sprites.get (s);
		    if (sprite == null)
		    {
			System.out.printf ("Null sprite %n");
		    }

		    sprite.destination.x = x;
		    sprite.destination.y = y;
		    sprite.destination.width = size;
		    sprite.destination.height = height;
		    x += w;
		}
	    }
	    else if (p.is ("on"))
	    {
		final Symbol upper = c.getTerms ().get (0);
		final Symbol lower = c.getTerms ().get (1);
		System.out.printf ("Simulate %s on %s %n", upper, lower);
		final Sprite upperSprite = sprites.get (upper);
		final Sprite lowerSprite = sprites.get (lower);
		upperSprite.destination.x = lowerSprite.destination.x;
		upperSprite.destination.y = lowerSprite.destination.y - upperSprite.destination.height;
	    }
	}
    }

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
	    final Node node = plan.getInitialNode ();
	    for (final Condition c : node.getAddConditions ())
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

    // public static void main (final String[] args)
    // {
    // final JFrame frame = new JFrame ("Blockworld Simulation");
    // frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);
    // final BlockworldSimulator planView = new BlockworldSimulator ();
    // planView.sprites.add (new Sprite (null, 5, 10, 100, 30, "sprite1"));
    // planView.sprites.add (new Sprite (null, 50, 100, 100, 100, "sprite2", "b", "c"));
    // frame.setSize (900, 500);
    // // frame.pack ();
    // frame.getContentPane ().add (planView);
    // frame.setVisible (true);
    // }

    @Override
    public void componentResized (final ComponentEvent e)
    {
	if (plan != null)
	{
	    setPlan (plan);
	}
	else
	{
	    sprites.clear ();
	}
    }

    @Override
    public void componentMoved (final ComponentEvent e)
    {
    }

    @Override
    public void componentShown (final ComponentEvent e)
    {
    }

    @Override
    public void componentHidden (final ComponentEvent e)
    {
    }
}
