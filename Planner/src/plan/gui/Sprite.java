
package plan.gui;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

import util.*;

public class Sprite extends Rectangle
{
    private static final int LINE_GAP = 2;
    private static final int MAX_FONT_SIZE = 14;
    private final List<Pair<String, Color>> labels = new ArrayList<Pair<String, Color>> ();
    private Color frameColor = Color.blue;
    private Color backColor = Color.white;
    private Color labelColor = Color.black;
    private Object target = null;
    public final Rectangle destination = new Rectangle (0, 0, 0, 0);

    public Sprite (final Object target, final int x, final int y, final int width, final int height, final String... lines)
    {
	this (target, lines);
	this.x = x;
	this.y = y;
	this.width = width;
	this.height = height;
	destination.setBounds (this);
    }

    public Sprite (final Object target, final int width, final int height, final String... lines)
    {
	this (target, lines);
	this.target = target;
	this.width = width;
	this.height = height;
	destination.setBounds (this);
    }

    public Sprite (final Object target, final String... lines)
    {
	this.target = target;
	for (final String l : lines)
	{
	    final Pair<String, Color> p = new Pair<String, Color> (l, null);
	    labels.add (p);
	}
    }

    public Sprite (final Object target, final List<String> lines)
    {
	this.target = target;
	for (final String l : lines)
	{
	    final Pair<String, Color> p = new Pair<String, Color> (l, null);
	    labels.add (p);
	}
    }

    public Object getTarget ()
    {
	return target;
    }

    public void paint (final Graphics g)
    {
	g.setColor (backColor);
	g.fillRect (x, y, width, height);
	g.setColor (labelColor);
	final int lineCount = labels.size ();
	final int lineHeight = Math.min (MAX_FONT_SIZE, (int)Math.round ((double)height / (double)lineCount) - LINE_GAP);
	final int top = (height - (lineHeight + LINE_GAP) * lineCount) / 2;
	final Font boldFont = FontUtil.getBoldFont (g, lineHeight);
	final Font font = FontUtil.getFont (g, lineHeight);
	g.setFont (boldFont);
	final FontMetrics metrics = g.getFontMetrics ();
	final int baseline = metrics.getAscent ();
	int yy = y + top;
	for (final Pair<String, Color> label : labels)
	{
	    final String l = label.getFirst ();
	    final Color c = label.getSecond ();
	    if (c == null)
	    {
		g.setColor (labelColor);
	    }
	    else
	    {
		g.setColor (c);
	    }
	    yy += LINE_GAP;
	    final int ww = metrics.stringWidth (l);
	    final int xx = x + (width - ww) / 2;
	    g.drawString (l, xx, yy + baseline);
	    yy += lineHeight;
	    g.setFont (font);
	}
	g.setColor (frameColor);
	g.drawRect (x, y, width - 1, height - 1);
    }

    public List<Pair<String, Color>> getLabels ()
    {
	return labels;
    }

    public void addLabel (final String fmt, final Object... args)
    {
	final String label = String.format (fmt, args);
	labels.add (new Pair<String, Color> (label, null));
    }

    public void addLabel (final Color color, final String fmt, final Object... args)
    {
	final String label = String.format (fmt, args);
	labels.add (new Pair<String, Color> (label, color));
    }

    public Color getFrameColor ()
    {
	return frameColor;
    }

    public void setFrameColor (final Color frameColor)
    {
	this.frameColor = frameColor;
    }

    public Color getBackColor ()
    {
	return backColor;
    }

    public void setBackColor (final Color backColor)
    {
	this.backColor = backColor;
    }

    public Color getLabelColor ()
    {
	return labelColor;
    }

    public void setLabelColor (final Color labelColor)
    {
	this.labelColor = labelColor;
    }

    public boolean isMoving ()
    {
	if (getX () != destination.x)
	{
	    return true;
	}
	if (getY () != destination.y)
	{
	    return true;
	}
	if (getWidth () != destination.width)
	{
	    return true;
	}
	if (getHeight () != destination.height)
	{
	    return true;
	}
	return false;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (target);
	buffer.append (">");
	return buffer.toString ();
    }
}
