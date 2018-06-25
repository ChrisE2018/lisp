
package lisp.describe;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.*;

import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.MultiMap;

public class InspectorPane extends JPanel
{
    private DescriberFactory factory;
    private Object subject;

    public InspectorPane (final Object subject)
    {
	setLayout (new GridBagLayout ());
	factory = new DescriberFactory ();
	this.subject = subject;
	addSubjectComponents ();
    }

    public DescriberFactory getFactory ()
    {
	return factory;
    }

    public void setFactory (final DescriberFactory factory)
    {
	this.factory = factory;
    }

    public Object getSubject ()
    {
	return subject;
    }

    public void setSubject (final Object subject)
    {
	this.subject = subject;
	removeAll ();
	addSubjectComponents ();
	revalidate ();
	repaint ();
    }

    private void addSubjectComponents ()
    {
	final Describer describer = factory.getDescriber (subject);
	final Package pkg = PackageFactory.getCurrentPackage ();
	int index = 0;
	final MultiMap<String, Object> description = describer.getDescriberValues (subject);
	for (final Entry<String, Collection<Object>> entry : description.entrySet ())
	{
	    // Make a symbol using the index value, i.e., d001
	    final String key = entry.getKey ();
	    final Set<Object> values = new LinkedHashSet<> (entry.getValue ());
	    for (final Object value : values)
	    {
		final MouseAdapter mouser = new MouseAdapter ()
		{
		    @Override
		    public void mouseClicked (final MouseEvent e)
		    {
			setSubject (value);
		    }
		};
		++index;
		final Describer valueDescriber = factory.getDescriber (value);
		final String valueString =
		    (valueDescriber == null) ? value.toString () : valueDescriber.getDescriberString (value);
		final String doc = describer.getDescriberDocumentation (subject, key);
		final Symbol symbol = pkg.internSymbol (String.format ("d%d", index));
		symbol.setValue (value);
		final GridBagConstraints c = new GridBagConstraints ();
		c.ipadx = 3;
		c.gridx = 0;
		c.gridy = index;
		final JLabel l1 = new JLabel (symbol.getName ());
		l1.addMouseListener (mouser);
		add (l1, c);
		c.gridx = 1;
		c.anchor = GridBagConstraints.LINE_END;
		final StringBuilder buffer = new StringBuilder ();
		if (value != null)
		{
		    final String className = value.getClass ().getSimpleName ();
		    if (!className.equals (key))
		    {
			buffer.append ("(");
			buffer.append (className);
			buffer.append (") ");
		    }
		}
		buffer.append (key);
		buffer.append (":");
		final JLabel l2 = new JLabel (buffer.toString ());
		l2.addMouseListener (mouser);
		final Font f = l2.getFont ();
		l2.setFont (f.deriveFont (f.getStyle () | Font.BOLD));
		add (l2, c);
		c.gridx = 2;
		c.anchor = GridBagConstraints.LINE_START;
		final JLabel l3 = new JLabel (valueString);
		l3.addMouseListener (mouser);
		add (l3, c);
		if (doc != null)
		{
		    c.gridx = 3;
		    final JLabel l4 = new JLabel (doc);
		    l4.addMouseListener (mouser);
		    add (l4, c);
		}
	    }
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
