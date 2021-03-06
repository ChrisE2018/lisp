
package plan.gui;

import static java.lang.Math.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.Timer;

import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.LogString;
import plan.*;
import util.FontUtil;

public class Simulator extends JPanel implements ComponentListener, MouseListener, MouseMotionListener, ActionListener
{
    private static final Logger LOGGER = Logger.getLogger (Simulator.class.getName ());
    private static final long ACTION_INTERVAL = 2000;
    private static final int ANIMATION_TIMER_INTERVAL = 1;
    private static final int SIMULATION_TIMER_INTERVAL = 250;

    /** Timer for animation. */
    private final Timer animationTimer = new AnimationTimer (ANIMATION_TIMER_INTERVAL);

    /** Timer for plan simulation. */
    private final Timer simulationTimer = new SimulationTimer (SIMULATION_TIMER_INTERVAL);

    /** Time of plan simulation action. */
    private long actionTimestamp;

    /** True while animation is active. Freezes plan simulation. */
    private boolean hasMovingSprite = true;

    /** Domain specific simulation content. */
    private final World world;

    private final JButton makePlan = new JButton ("Make Plan");
    private final JButton simulate = new JButton ("Simulate");

    /** The plan to simulate */
    private Plan plan;

    /** Plan steps that have already been shown. */
    private final List<Node> simulatedNodes = new ArrayList<> ();

    /** Goals of the original plan. */
    private final List<Condition> goals = new ArrayList<> ();

    /** Conditions that are currently true. */
    private final List<Condition> state = new ArrayList<> ();

    /** Blocks found in the plan. */
    private final Set<Symbol> blocks = new LinkedHashSet<> ();

    /** Blocks that have been given a display position. */
    private final Set<Symbol> positionedBlocks = new LinkedHashSet<> ();

    /** Sprites for each block. */
    private final Map<Symbol, Sprite> sprites = new HashMap<> ();

    private int blockSpacing = 100;
    private int blockWidth = 25;
    private int blockHeight = 25;
    private int tableY = 250;

    public Simulator (final World world)
    {
	setLayout (null);
	this.world = world;
	world.setSimulator (this);
	addComponentListener (this);
	addMouseListener (this);
	addMouseMotionListener (this);
	makePlan.addActionListener (this);
	simulate.addActionListener (this);
	SwingUtilities.invokeLater (new Runnable ()
	{
	    @Override
	    public void run ()
	    {
		makePlan.setSize (100, 30);
		simulate.setSize (100, 30);
		add (makePlan);
		add (simulate);
		animationTimer.start ();
		simulationTimer.start ();
	    }
	});
    }

    public boolean isPositioned (final Symbol s)
    {
	return positionedBlocks.contains (s);
    }

    public void setPositioned (final Symbol s)
    {
	positionedBlocks.add (s);
    }

    public Set<Symbol> getPositioned ()
    {
	return positionedBlocks;
    }

    public Sprite getSprite (final Symbol s)
    {
	return sprites.get (s);
    }

    private void setPlan (final Plan plan)
    {
	LOGGER.info (new LogString ("setPlan %s", plan));
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
		blocks.addAll (world.getObjects (c));
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

    /** Calculates baseline when the view is resized this */
    private void updateSizes ()
    {
	blockWidth = Math.max (50, Math.min (getWidth (), getHeight ()) / 10);
	blockSpacing = blockWidth + blockWidth / 4;
	blockHeight = blockWidth;
	final int height3 = getHeight () / 3;
	final int baseline = height3 * 2;
	tableY = Math.max (10, baseline - blockHeight + 10);
	world.setBlockWidth (blockWidth);
	world.setBlockHeight (blockHeight);
	world.setBlockSpacing (blockSpacing);
	world.setTableY (tableY);

	makePlan.setLocation (getWidth () - 150, 10);
	simulate.setLocation (getWidth () - 150, 50);
    }

    private class AnimationTimer extends Timer
    {
	AnimationTimer (final int interval)
	{
	    super (interval, new ActionListener ()
	    {
		@Override
		public void actionPerformed (final ActionEvent e)
		{
		    boolean moved = false;
		    for (final Sprite sprite : sprites.values ())
		    {
			final int dx = sprite.destination.x - sprite.x;
			final int dy = sprite.destination.y - sprite.y;
			final int dw = sprite.destination.width - sprite.width;
			final int dh = sprite.destination.height - sprite.height;
			if (dx != 0 || dy != 0 || dw != 0 || dh != 0)
			{
			    moved = true;
			    sprite.x += sign (dx);
			    sprite.y += sign (dy);
			    sprite.width += sign (dw);
			    sprite.height += sign (dh);
			}
		    }
		    if (moved)
		    {
			repaint ();
		    }
		    hasMovingSprite = moved;
		}
	    });
	}
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

    private class SimulationTimer extends Timer
    {
	SimulationTimer (final int interval)
	{
	    super (interval, new ActionListener ()
	    {
		@Override
		public void actionPerformed (final ActionEvent e)
		{
		    if (!hasMovingSprite && actionTimestamp + ACTION_INTERVAL < System.currentTimeMillis ())
		    {
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
			actionTimestamp = System.currentTimeMillis ();
		    }
		}
	    });
	}
    }

    /** Get nodes with no predecessors that have not been simulated. These can be simulated now. */
    private List<Node> getFirstNodes ()
    {
	final List<Node> result = new ArrayList<> ();
	for (final Node node : plan.getNodes ())
	{
	    if (!simulatedNodes.contains (node))
	    {
		if (!hasUnmarkedPredecessor (node, simulatedNodes))
		{
		    result.add (node);
		}
	    }
	}
	return result;
    }

    private boolean hasUnmarkedPredecessor (final Node node, final Collection<Node> marked)
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
	LOGGER.info ("==========================================");
	LOGGER.info (new LogString ("[%d/%d] Simulate %s", simulatedNodes.size (), plan.getNodes ().size (), node));
	for (final Condition c : node.getDeleteConditions ())
	{
	    state.remove (c);
	}
	for (final Condition c : node.getAddConditions ())
	{
	    state.add (c);
	}
	LOGGER.info (new LogString ("State %s", state));
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
	    LOGGER.info (new LogString ("Constraints %s", buffer));
	}
	for (final Condition c : node.getAddConditions ())
	{
	    if (world.canChange (c))
	    {
		positionedBlocks.removeAll (c.getTerms ());
	    }
	}
	animateCurrentState ();
	simulatedNodes.add (node);
    }

    private void animateCurrentState ()
    {
	LOGGER.info (new LogString ("Animate %s", state));
	for (final Symbol b : blocks)
	{
	    if (!positionedBlocks.contains (b))
	    {
		LOGGER.info (new LogString ("Position block %s", b));
	    }
	}
	final int limit = state.size ();
	for (int i = 0; positionedBlocks.size () < blocks.size () && i < limit; i++)
	{
	    for (final Condition c : state)
	    {
		LOGGER.info (new LogString ("Animate %s", c));
		world.animateCondition (c);
	    }
	}
    }

    @Override
    public void actionPerformed (final ActionEvent e)
    {
	try
	{
	    final Object source = e.getSource ();
	    if (source == makePlan)
	    {
		makePlan ();
	    }
	    if (source == simulate)
	    {
		simulatePlan ();
	    }
	}
	catch (final Throwable ex)
	{
	    ex.printStackTrace ();
	}
    }

    private void makePlan ()
    {
	final Package pkg = PackageFactory.getDefaultPackage ();
	final Node initialNode = plan.getInitialNode ();
	final Plan p = new Plan (plan.getName ().gensym ());
	final Node start = new Node (pkg.internSymbol ("start").gensym ());
	final Node goal = new Node (pkg.internSymbol ("goal").gensym ());
	start.addSuccessor (goal);
	start.getAddConditions ().addAll (initialNode.getAddConditions ());
	goal.getGoalConditions ().addAll (goals);
	p.addNode (start);
	p.addNode (goal);

    }

    private void simulatePlan ()
    {

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
		LOGGER.info (new LogString ("mousePressed on %s", mouseSprite));
	    }
	}
    }

    @Override
    public void mouseReleased (final MouseEvent e)
    {
	if (mouseSprite != null)
	{
	    // LOGGER.info (new LogString ("mouseReleased on %s", mouseSprite));
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
	LOGGER.info (new LogString ("Sprite %s [%s, %s] is above table %s", block, blockSprite.x, blockSprite.y, result));
	for (final Entry<Symbol, Sprite> entry : sprites.entrySet ())
	{
	    final Sprite sprite = entry.getValue ();
	    if (blockSprite != sprite)
	    {
		if (blockSprite.getMaxY () <= sprite.getY ())
		{
		    final double top = sprite.getMinY () - blockHeight;
		    LOGGER.info (new LogString ("Sprite %s [x %s, %s] is above %s [x %s, %s] [y %s %s]", block, blockSprite.x,
		            blockSprite.y, entry.getKey (), sprite.x, sprite.y, top, sprite.getMaxY ()));
		    if (top < result)
		    {
			if (blockSprite.getX () <= sprite.getMaxX ())
			{
			    if (blockSprite.getMaxX () >= sprite.getMinX ())
			    {
				result = top;
				LOGGER.info (new LogString ("Sprite %s will fall onto %s at %s", block, entry.getKey (), top));
			    }
			}
		    }
		}
	    }
	}
	LOGGER.info (new LogString ("Sprite %s [%s, %s] will fall to %s", block, blockSprite.x, blockSprite.y, result));
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
	    // LOGGER.info (new LogString ("mouseDragged on %s", mouseSprite));
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

    public static void makeView (final World world, final Plan plan)
    {
	if (plan == null)
	{
	    LOGGER.info ("Can't view null plan");
	    return;
	}
	JFrame frame = simulationViews.get (plan);
	if (frame == null)
	{
	    frame = new JFrame ("" + plan.getName () + " Simulation");
	    frame.setDefaultCloseOperation (WindowConstants.HIDE_ON_CLOSE);
	    final Simulator simulatorView = new Simulator (world);
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
