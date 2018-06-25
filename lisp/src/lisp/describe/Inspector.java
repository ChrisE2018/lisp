
package lisp.describe;

import java.awt.*;

import javax.swing.JFrame;

public class Inspector
{
    private DescriberFactory factory;

    public Inspector ()
    {
	factory = new DescriberFactory ();
    }

    public Inspector (final DescriberFactory factory)
    {
	this.factory = factory != null ? factory : new DescriberFactory ();
    }

    public DescriberFactory getFactory ()
    {
	return factory;
    }

    public void setFactory (final DescriberFactory factory)
    {
	this.factory = factory;
    }

    public Object inspect (final Object arg)
    {
	final JFrame inspector = new JFrame ("Inspector");
	inspector.setContentPane (new InspectorPane (arg));
	inspector.pack ();
	centreWindow (inspector);
	inspector.setVisible (true);
	return arg;
    }

    public static void centreWindow (final Window frame)
    {
	final Dimension dimension = Toolkit.getDefaultToolkit ().getScreenSize ();
	final int x = (int)((dimension.getWidth () - frame.getWidth ()) / 2);
	final int y = (int)((dimension.getHeight () - frame.getHeight ()) / 2);
	frame.setLocation (x, y);
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
