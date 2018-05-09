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

import java.awt.Container;

import javax.swing.*;

import org.abego.treelayout.*;
import org.abego.treelayout.util.*;

import lisp.Symbol;
import plan.Plan;

/**
 * Demonstrates how to use the {@link TreeLayout} to render a tree in a Swing application.
 * <p>
 * Intentionally the sample code is kept simple. I.e. it does not include stuff like anti-aliasing
 * and other stuff one would add to make the output look nice.
 * <p>
 * Screenshot:
 * <p>
 * <img src="doc-files/swingdemo.png" alt="A tree rendered using Swing">
 *
 * @author Udo Borkowski (ub@abego.org)
 */
public class PlanTreeDemo
{
    private static void showInDialog (final JComponent panel)
    {
	final JDialog dialog = new JDialog ();
	final Container contentPane = dialog.getContentPane ();
	((JComponent)contentPane).setBorder (BorderFactory.createEmptyBorder (10, 10, 10, 10));
	contentPane.add (panel);
	dialog.pack ();
	dialog.setLocationRelativeTo (null);
	dialog.setVisible (true);
    }

    private static TreeForTreeLayout<Plan> getSampleTree (final Plan plan)
    {
	final TreeForTreeLayout<Plan> tree = createSampleTree (plan);
	return tree;
    }

    /**
     * @return a "Sample" tree with {@link Plan} items as nodes.
     */
    private static TreeForTreeLayout<Plan> createSampleTree (final Plan root)
    {
	final DefaultTreeForTreeLayout<Plan> tree = new DefaultTreeForTreeLayout<Plan> (root);
	createPlanTree (tree, root);
	return tree;
    }

    private static void createPlanTree (final DefaultTreeForTreeLayout<Plan> tree, final Plan root)
    {
	for (final Symbol childName : root.getChildren ())
	{
	    final Plan child = (Plan)childName.getValue ();
	    tree.addChild (root, child);
	    createPlanTree (tree, child);
	}
    }

    /**
     * Shows a dialog with a tree in a layout created by {@link TreeLayout}, using the Swing
     * component {@link PlanTreePane}.
     *
     * @param args args[0]: treeName (default="")
     */
    public static void displayPlan (final Plan plan)
    {
	// get the sample tree
	final TreeForTreeLayout<Plan> tree = getSampleTree (plan);

	// setup the tree layout configuration
	final double gapBetweenLevels = 50;
	final double gapBetweenNodes = 10;
	final DefaultConfiguration<Plan> configuration = new DefaultConfiguration<Plan> (gapBetweenLevels, gapBetweenNodes);

	// create the NodeExtentProvider for Plan nodes
	final NodeExtentProvider<Plan> nodeExtentProvider = new NodeExtentProvider<Plan> ()
	{
	    @Override
	    public double getWidth (final Plan treeNode)
	    {
		return 60;
	    }

	    @Override
	    public double getHeight (final Plan treeNode)
	    {
		return 40;
	    }
	};

	// create the layout
	final TreeLayout<Plan> treeLayout = new TreeLayout<Plan> (tree, nodeExtentProvider, configuration);

	// Create a panel that draws the nodes and edges and show the panel
	final PlanInBoxTreePane panel = new PlanInBoxTreePane (treeLayout);
	final JScrollPane scroll =
	    new JScrollPane (panel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	showInDialog (scroll);
    }
}
