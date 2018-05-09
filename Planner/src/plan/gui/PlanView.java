
package plan.gui;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import plan.*;

public class PlanView extends JPanel implements ComponentListener
{
    private final PlanLayout planLayout = new PlanLayout ();
    private Plan plan;

    private List<Sprite> sprites = new ArrayList<Sprite> ();

    public PlanView ()
    {
	addComponentListener (this);
    }

    @Override
    public void paintComponent (final Graphics g)
    {
	g.setColor (Color.black);
	g.fillRect (0, 0, getWidth (), getHeight ());
	g.setColor (Color.YELLOW);
	for (final Sprite sprite : sprites)
	{
	    final Object target = sprite.getTarget ();
	    if (target instanceof Node)
	    {
		final Node node = (Node)target;
		final int x1 = (int)sprite.getCenterX ();
		final int y1 = (int)sprite.getCenterY ();
		final List<Node> next = node.getNext ();
		for (final Node n : next)
		{
		    final Sprite s = getSprite (n);
		    final int x2 = (int)s.getCenterX ();
		    final int y2 = (int)s.getCenterY ();
		    g.drawLine (x1, y1, x2, y2);
		}
	    }
	}
	for (final Sprite sprite : sprites)
	{
	    sprite.paint (g);
	}
    }

    public Plan getPlan ()
    {
	return plan;
    }

    public void setPlan (final Plan plan)
    {
	this.plan = plan;
	sprites = planLayout.getLayout (plan, getBounds ());
	repaint ();
    }

    private Sprite getSprite (final Node node)
    {
	for (final Sprite sprite : sprites)
	{
	    if (sprite.getTarget () == node)
	    {
		return sprite;
	    }
	}
	return null;
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

    private static Map<Plan, JFrame> planViews = new HashMap<Plan, JFrame> ();

    public static void makeView (final Plan plan)
    {
	JFrame frame = planViews.get (plan);
	if (frame == null)
	{
	    frame = new JFrame (plan.getName ().getName ());
	    frame.setDefaultCloseOperation (JFrame.HIDE_ON_CLOSE);
	    final PlanView planView = new PlanView ();
	    frame.setSize (900, 500);
	    // frame.pack ();
	    frame.getContentPane ().add (planView);
	    planView.setPlan (plan);
	    planViews.put (plan, frame);
	}
	frame.setVisible (true);
	final JFrame f = frame;
	java.awt.EventQueue.invokeLater (new Runnable ()
	{
	    @Override
	    public void run ()
	    {
		f.toFront ();
		f.repaint ();
	    }
	});

    }

    public static void main (final String[] args)
    {
	final JFrame frame = new JFrame ("Demo");
	frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);
	final PlanView planView = new PlanView ();
	planView.sprites.add (new Sprite (null, 5, 10, 100, 30, "sprite1"));
	planView.sprites.add (new Sprite (null, 50, 100, 100, 100, "sprite2", "b", "c"));
	frame.setSize (900, 500);
	// frame.pack ();
	frame.getContentPane ().add (planView);
	frame.setVisible (true);
    }

    @Override
    public void componentResized (final ComponentEvent e)
    {
	if (plan != null)
	{
	    sprites = planLayout.getLayout (plan, getBounds ());
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
