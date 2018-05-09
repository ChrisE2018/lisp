/*
 * [The "BSD license"]
 * Copyright (c) 2011, abego Software GmbH, Germany (http://www.abego.org)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the abego Software GmbH nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package plan.gui;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.util.Map.Entry;

import javax.swing.JComponent;

import org.abego.treelayout.*;

import plan.Plan;
import search.SearchState;

/**
 * A JComponent displaying a tree of TextInBoxes, given by a {@link TreeLayout}.
 *
 * @author Udo Borkowski (ub@abego.org)
 */
public class PlanInBoxTreePane extends JComponent implements MouseListener
{
    private final TreeLayout<Plan> treeLayout;

    private TreeForTreeLayout<Plan> getTree ()
    {
	return treeLayout.getTree ();
    }

    private Iterable<Plan> getChildren (final Plan parent)
    {
	return getTree ().getChildren (parent);
    }

    private Rectangle2D.Double getBoundsOfNode (final Plan node)
    {
	return treeLayout.getNodeBounds ().get (node);
    }

    /**
     * Specifies the tree to be displayed by passing in a {@link TreeLayout} for that tree.
     *
     * @param treeLayout the {@link TreeLayout} to be displayed
     */
    public PlanInBoxTreePane (final TreeLayout<Plan> treeLayout)
    {
	this.treeLayout = treeLayout;

	addMouseListener (this);
	final Dimension size = treeLayout.getBounds ().getBounds ().getSize ();
	setPreferredSize (size);
    }

    // -------------------------------------------------------------------
    // painting

    private final static int ARC_SIZE = 10;
    private final static Color BOX_COLOR = Color.orange;
    private final static Color BORDER_COLOR = Color.darkGray;
    private final static Color TEXT_COLOR = Color.black;

    private void paintEdges (final Graphics g, final Plan parent)
    {
	if (!getTree ().isLeaf (parent))
	{
	    final Rectangle2D.Double b1 = getBoundsOfNode (parent);
	    final double x1 = b1.getCenterX ();
	    final double y1 = b1.getCenterY ();
	    for (final Plan child : getChildren (parent))
	    {
		final Rectangle2D.Double b2 = getBoundsOfNode (child);
		g.drawLine ((int)x1, (int)y1, (int)b2.getCenterX (), (int)b2.getCenterY ());

		paintEdges (g, child);
	    }
	}
    }

    private void paintBox (final Graphics g, final Plan plan)
    {
	// draw the box in the background
	final Color boxColor = (plan.solved ()) ? Color.green : BOX_COLOR;

	g.setColor (boxColor);
	final Rectangle2D.Double box = getBoundsOfNode (plan);
	g.fillRoundRect ((int)box.x, (int)box.y, (int)box.width - 1, (int)box.height - 1, ARC_SIZE, ARC_SIZE);
	g.setColor (BORDER_COLOR);
	g.drawRoundRect ((int)box.x, (int)box.y, (int)box.width - 1, (int)box.height - 1, ARC_SIZE, ARC_SIZE);

	// draw the text on top of the box (possibly multiple lines)
	g.setColor (TEXT_COLOR);

	final String[] lines = plan.getName ().getName ().split ("\n");
	final FontMetrics m = getFontMetrics (getFont ());
	final int x = (int)box.x + ARC_SIZE / 2;
	int y = (int)box.y + m.getAscent () + m.getLeading () + 1;
	for (int i = 0; i < lines.length; i++)
	{
	    g.drawString (lines[i], x, y);
	    y += m.getHeight ();
	}
	final SearchState searchState = plan.getSearchState ();
	if (searchState != null)
	{
	    final StringBuilder buffer = new StringBuilder ();
	    final double a = searchState.getCost ();
	    final double b = plan.estimateRemainingCost ();
	    final double c = a + b;
	    final String s = String.format ("%.1f = %.1f + %.1f", c, a, b);
	    g.drawString (s, x, y);
	    y += m.getHeight ();
	}
	else
	{
	    g.drawString ("C = " + plan.estimateRemainingCost (), x, y);
	    y += m.getHeight ();
	}
    }

    @Override
    public void paint (final Graphics g)
    {
	super.paint (g);

	paintEdges (g, getTree ().getRoot ());

	// paint the boxes
	for (final Plan plan : treeLayout.getNodeBounds ().keySet ())
	{
	    paintBox (g, plan);
	}
    }

    @Override
    public void mouseClicked (final MouseEvent e)
    {
	for (final Entry<Plan, Rectangle2D.Double> entry : treeLayout.getNodeBounds ().entrySet ())
	{
	    final Plan plan = entry.getKey ();
	    final Rectangle2D.Double bounds = entry.getValue ();
	    if (bounds.contains (e.getPoint ()))
	    {
		System.out.printf ("Click on %s %n", plan);
		PlanView.makeView (plan);
	    }
	}
    }

    @Override
    public void mousePressed (final MouseEvent e)
    {
    }

    @Override
    public void mouseReleased (final MouseEvent e)
    {
    }

    @Override
    public void mouseEntered (final MouseEvent e)
    {
    }

    @Override
    public void mouseExited (final MouseEvent e)
    {
    }
}
