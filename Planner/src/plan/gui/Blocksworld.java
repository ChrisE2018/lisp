
package plan.gui;

import java.awt.Color;
import java.lang.reflect.Field;
import java.util.*;
import java.util.logging.Logger;

import lisp.lang.Symbol;
import lisp.util.LogString;
import plan.Condition;

public class Blocksworld implements World
{
    // TODO Consider moving actions into the current World
    private static final Logger LOGGER = Logger.getLogger (Blocksworld.class.getName ());

    private Simulator simulator;
    private int blockSpacing = 100;
    private int blockWidth = 25;
    private int blockHeight = 25;
    private int tableY = 250;

    public void setSimulator (final Simulator simulator)
    {
	this.simulator = simulator;
    }

    public int getBlockSpacing ()
    {
	return blockSpacing;
    }

    public void setBlockSpacing (final int blockSpacing)
    {
	this.blockSpacing = blockSpacing;
    }

    public int getBlockWidth ()
    {
	return blockWidth;
    }

    public void setBlockWidth (final int blockWidth)
    {
	this.blockWidth = blockWidth;
    }

    public int getBlockHeight ()
    {
	return blockHeight;
    }

    public void setBlockHeight (final int blockHeight)
    {
	this.blockHeight = blockHeight;
    }

    public int getTableY ()
    {
	return tableY;
    }

    public void setTableY (final int tableY)
    {
	this.tableY = tableY;
    }

    public boolean canChange (final Condition c)
    {
	// Can implement this by inspecting the available actions
	return !c.getPredicate ().is ("color");
    }

    public Collection<? extends Symbol> getObjects (final Condition c)
    {
	final List<Symbol> result = new ArrayList<> ();
	if (c.getPredicate ().is ("color"))
	{
	    result.add (c.getTerms ().get (0));
	}
	else
	{
	    result.addAll (c.getTerms ());
	}
	return result;
    }

    public void animateCondition (final Condition c)
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
	if (!simulator.isPositioned (s))
	{
	    LOGGER.info (new LogString ("Sim %s", c));
	    final Sprite sprite = simulator.getSprite (s);
	    final int x = getOpenTableX (sprite);
	    sprite.destination.x = x;
	    sprite.destination.y = tableY;
	    sprite.destination.width = blockWidth;
	    sprite.destination.height = blockHeight;
	    LOGGER.info (new LogString ("Table %s %s at %s", s, blockWidth, x));
	    simulator.setPositioned (s);
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
	for (final Symbol b : simulator.getPositioned ())
	{
	    final Sprite sprite = simulator.getSprite (b);
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
	if (!simulator.isPositioned (upper))
	{
	    if (simulator.isPositioned (lower))
	    {
		LOGGER.info (new LogString ("Sim %s", c));
		final Sprite upperSprite = simulator.getSprite (upper);
		final Sprite lowerSprite = simulator.getSprite (lower);
		LOGGER.info (new LogString ("Simulate %s on %s", upper, lower));
		upperSprite.destination.x = lowerSprite.destination.x;
		upperSprite.destination.y = lowerSprite.destination.y - blockHeight;
		upperSprite.destination.width = blockWidth;
		upperSprite.destination.height = blockHeight;
		simulator.setPositioned (upper);
	    }
	}
    }

    private void setBlockColor (final Condition c)
    {
	try
	{
	    LOGGER.info (new LogString ("Sim %s", c));
	    final Symbol block = c.getTerms ().get (0);
	    final Sprite sprite = simulator.getSprite (block);
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
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
