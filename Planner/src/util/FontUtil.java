
package util;

import java.awt.*;

public class FontUtil
{
    /** Derive a font from the current font at a new size. */
    public static Font getFont (final Graphics g, final int size)
    {
	final String fontName = g.getFont ().getFontName ();
	return new Font (fontName, 0, size);
    }

    /** Derive a bold font from the current font at a new size. */
    public static Font getBoldFont (final Graphics g, final int size)
    {
	final String fontName = g.getFont ().getFontName ();
	return new Font (fontName, Font.BOLD, size);
    }

    /** Derive an italic font from the current font at a new size. */
    public static Font getItalicFont (final Graphics g, final int size)
    {
	final String fontName = g.getFont ().getFontName ();
	return new Font (fontName, Font.ITALIC, size);
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
