
package plan.gui;

import static java.lang.Math.*;

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

public class BlockworldSimulator extends JPanel implements ActionListener, ComponentListener, MouseListener, MouseMotionListener
{
    private static final long ACTION_INTERVAL = 2000;
    private Plan plan;

    private final List<Node> simulatedNodes = new ArrayList<> ();

    private long actionTimestamp;

    private final Set<Symbol> blocks = new LinkedHashSet<> ();
    private final Set<Symbol> positionedBlocks = new LinkedHashSet<> ();

    private final Map<Symbol, Sprite> sprites = new HashMap<> ();

    private final Timer timer = new Timer (1, this);

    /** Conditions that are currently true. */
    private final List<Condition> state = new ArrayList<> ();

    /** Goals of the original plan. */
    private final List<Condition> goals = new ArrayList<> ();

    private int blockSpacing = 100;
    private int blockWidth = 25;
    private int blockHeight = 25;
    private int tableY = 250;

    public BlockworldSimulator ()
    {
	timer.start ();
	addComponentListener (this);
	addMouseListener (this);
	addMouseMotionListener (this);
    }

    private void updateSizes ()
    {
	blockWidth = Math.max (50, Math.min (getWidth (), getHeight ()) / 10);
	blockSpacing = blockWidth + blockWidth / 4;
	blockHeight = blockWidth;
	final int height3 = getHeight () / 3;
	final int baseline = height3 * 2;
	tableY = Math.max (10, baseline - blockHeight + 10);
    }

    /** Execute a timer action. */
    @Override
    public void actionPerformed (final ActionEvent e)
    {
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
	final long now = System.currentTimeMillis ();
	if (actionTimestamp + ACTION_INTERVAL < now)
	{
	    for (final Sprite sprite : sprites.values ())
	    {
		if (sprite.isMoving ())
		{
		    return;
		}
	    }
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
		// System.out.printf ("Simulation complete %n");
		// timer.stop ();
	    }
	    actionTimestamp = now;
	}
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
	final List<Node> result = new ArrayList<> ();
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

    private void setPlan (final Plan plan)
    {
	System.out.printf ("setPlan %s%n", plan);
	if (this.plan != plan)
	{
	    this.plan = plan;
	    blocks.clear ();
	    positionedBlocks.clear ();
	    sprites.clear ();
	    simulatedNodes.clear ();
	    final Node initialNode = plan.getInitialNode ();
	    for (final Condition c : initialNode.getAddConditions ())
	    {
		if (c.getPredicate ().is ("color"))
		{
		    blocks.add (c.getTerms ().get (0));
		}
		else
		{
		    blocks.addAll (c.getTerms ());
		}
	    }
	    int i = 0;
	    for (final Symbol s : blocks)
	    {
		final Sprite sprite = new Sprite (s, 100 + i * 15, 50 + i * 10, 10, 10, s.getName ());
		sprites.put (s, sprite);
		i++;
	    }
	    final List<Node> goalNodes = plan.getOriginalGoalNodes ();
	    goals.clear ();
	    for (final Node node : goalNodes)
	    {
		goals.addAll (node.getGoalConditions ());
	    }
	    actionTimestamp = System.currentTimeMillis ();
	}
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
	for (final Condition c : node.getAddConditions ())
	{
	    if (!c.getPredicate ().is ("color"))
	    {
		positionedBlocks.removeAll (c.getTerms ());
	    }
	}
	update ();
	simulatedNodes.add (node);
    }

    private void update ()
    {
	for (final Symbol b : blocks)
	{
	    if (!positionedBlocks.contains (b))
	    {
		System.out.printf ("Position block %s %n", b);
	    }
	}
	final int limit = state.size ();
	for (int i = 0; positionedBlocks.size () < blocks.size () && i < limit; i++)
	{
	    for (final Condition c : state)
	    {
		update (c);
	    }
	}
    }

    private void update (final Condition c)
    {
	final Symbol p = c.getPredicate ();
	if (p.is ("ontable"))
	{
	    showBlockOnTable (c);
	}
	else if (p.is ("on"))
	{
	    showBlockOnBlock (c);
	}
	else if (p.is ("color"))
	{
	    setBlockColor (c);
	}
    }

    private void showBlockOnTable (final Condition c)
    {
	final Symbol s = c.getTerms ().get (0);
	if (!positionedBlocks.contains (s))
	{
	    final Sprite sprite = sprites.get (s);
	    final int x = getOpenTableX (sprite);
	    sprite.destination.x = x;
	    sprite.destination.y = tableY;
	    sprite.destination.width = blockWidth;
	    sprite.destination.height = blockHeight;
	    System.out.printf ("Table %s %s at %s %n", s, blockWidth, x);
	    positionedBlocks.add (s);
	}
    }

    private int getOpenTableX (final Sprite sprite)
    {
	final int originalX = sprite.destination.x;
	if (isTableXAvailable (originalX))
	{
	    return originalX;
	}
	for (int x = blockWidth / 10; true; x += blockSpacing)
	{
	    if (isTableXAvailable (x))
	    {
		return x;
	    }
	}
    }

    private boolean isTableXAvailable (final int x)
    {
	final int low = x - blockSpacing;
	final int high = x + blockSpacing;
	for (final Symbol b : positionedBlocks)
	{
	    final Sprite sprite = sprites.get (b);
	    if (sprite.destination.x > low && sprite.destination.x < high)
	    {
		return false;
	    }
	}
	return true;
    }

    private void showBlockOnBlock (final Condition c)
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
		upperSprite.destination.y = lowerSprite.destination.y - blockHeight;
		upperSprite.destination.width = blockWidth;
		upperSprite.destination.height = blockHeight;
		positionedBlocks.add (upper);
	    }
	}
    }

    private void setBlockColor (final Condition c)
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

    @Override
    public void paintComponent (final Graphics g)
    {
	final int baseline = (getHeight () * 2) / 3;
	g.setColor (Color.green);
	g.fillRect (0, 0, getWidth (), baseline);
	g.setColor (Color.darkGray);
	g.fillRect (0, baseline, getWidth (), getHeight () - baseline);
	// g.setColor (Color.YELLOW);
	// g.fillOval (100, 100, 50, 50);
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

    @Override
    public void componentResized (final ComponentEvent e)
    {
	updateSizes ();
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

    private Sprite mouseSprite = null;
    private Symbol mouseBlock = null;
    private int mouseDeltaX = 0;
    private int mouseDeltaY = 0;

    @Override
    public void mouseClicked (final MouseEvent e)
    {
    }

    @Override
    public void mousePressed (final MouseEvent e)
    {
	for (final Entry<Symbol, Sprite> entry : sprites.entrySet ())
	{
	    final Sprite sprite = entry.getValue ();
	    if (sprite.contains (e.getPoint ()))
	    {
		mouseSprite = sprite;
		mouseBlock = entry.getKey ();
		mouseDeltaX = e.getX () - sprite.x;
		mouseDeltaY = e.getY () - sprite.y;
		System.out.printf ("mousePressed on %s %n", mouseSprite);
	    }
	}
    }

    @Override
    public void mouseReleased (final MouseEvent e)
    {
	if (mouseSprite != null)
	{
	    // System.out.printf ("mouseReleased on %s %n", mouseSprite);
	    mouseSprite.x = e.getX () - mouseDeltaX;
	    mouseSprite.y = min (e.getY () - mouseDeltaY, tableY);
	    mouseSprite.destination.x = mouseSprite.x;
	    mouseSprite.destination.y = getLandingLevel (mouseBlock);
	    mouseSprite = null;
	    mouseBlock = null;
	}
    }

    private int getLandingLevel (final Symbol block)
    {
	double result = tableY;
	final Sprite blockSprite = sprites.get (block);
	System.out.printf ("Sprite %s [%s, %s] is above table %s%n", block, blockSprite.x, blockSprite.y, result);
	for (final Entry<Symbol, Sprite> entry : sprites.entrySet ())
	{
	    final Sprite sprite = entry.getValue ();
	    if (blockSprite != sprite)
	    {
		if (blockSprite.getMaxY () <= sprite.getY ())
		{
		    final double top = sprite.getMinY () - blockHeight;
		    System.out.printf ("Sprite %s [x %s, %s] is above %s [x %s, %s] [y %s %s] %n", block, blockSprite.x,
		            blockSprite.y, entry.getKey (), sprite.x, sprite.y, top, sprite.getMaxY ());
		    if (top < result)
		    {
			if (blockSprite.getX () <= sprite.getMaxX ())
			{
			    if (blockSprite.getMaxX () >= sprite.getMinX ())
			    {
				result = top;
				System.out.printf ("Sprite %s will fall onto %s at %s %n", block, entry.getKey (), top);
			    }
			}
		    }
		}
	    }
	}
	System.out.printf ("Sprite %s [%s, %s] will fall to %s%n", block, blockSprite.x, blockSprite.y, result);
	return (int)round (result);
    }

    @Override
    public void mouseEntered (final MouseEvent e)
    {
    }

    @Override
    public void mouseExited (final MouseEvent e)
    {
	mouseSprite = null;
    }

    @Override
    public void mouseDragged (final MouseEvent e)
    {
	if (mouseSprite != null)
	{
	    // System.out.printf ("mouseDragged on %s %n", mouseSprite);
	    mouseSprite.destination.x = e.getX () - mouseDeltaX;
	    mouseSprite.destination.y = min (e.getY () - mouseDeltaY, tableY);
	}
    }

    @Override
    public void mouseMoved (final MouseEvent e)
    {
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

    private static Map<Plan, JFrame> simulationViews = new HashMap<> ();

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
	    frame.setDefaultCloseOperation (WindowConstants.HIDE_ON_CLOSE);
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
