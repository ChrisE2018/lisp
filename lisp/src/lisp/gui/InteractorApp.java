
package lisp.gui;

import java.awt.Dimension;
import java.awt.event.*;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.logging.LogManager;

import javax.swing.*;

import lisp.cc.VerifyPrimitives;
import lisp.eval.*;
import lisp.lang.*;

public class InteractorApp
{
    private static final String WINDOW_NAME = "Interactor";
    private final Interpreter interpreter;
    private final InteractorPane interactor;

    public static void main (final String[] args)
    {
	final PrintStream err = System.err;
	try
	{
	    final InteractorApp app = new InteractorApp (args);
	    app.open (WINDOW_NAME, true);
	}
	catch (final InvocationTargetException e)
	{
	    Throwable cause = e;
	    for (int i = 0; i < 10 && cause instanceof InvocationTargetException; i++)
	    {
		cause = cause.getCause ();
	    }
	    err.printf ("Initialization error %s %n", cause);
	    cause.printStackTrace (err);
	}
	catch (final Exception e)
	{
	    e.printStackTrace (err);
	}
    }

    private InteractorApp (final String[] args) throws Exception
    {
	LogManager.getLogManager ()
	        .readConfiguration (Interactor.class.getResource ("loggingBootstrap.properties").openStream ());
	interpreter = new Interpreter ();
	final Application application = new Application (interpreter);
	application.initialize (args);
	interactor = new InteractorPane (interpreter);
    }

    private JFrame open (final String title, final boolean visible)
    {
	final JFrame frame = new JFrame (title);
	frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);

	// Put the editor pane in a scroll pane.
	final JScrollPane scrollPane = new JScrollPane (interactor);
	scrollPane.setVerticalScrollBarPolicy (JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	scrollPane.setPreferredSize (new Dimension (700, 400));
	scrollPane.setMinimumSize (new Dimension (100, 50));
	addMenubar (frame);
	frame.setContentPane (scrollPane);
	frame.pack ();
	frame.setLocationRelativeTo (null);
	if (visible)
	{
	    frame.setVisible (visible);
	}
	return frame;
    }

    @SuppressWarnings ("unchecked")
    private void addMenubar (final JFrame frame)
    {
	final JMenuBar menubar = new JMenuBar ();
	final Symbol menuSymbol = PackageFactory.getSystemPackage ().internSymbol ("*menus*");
	final List<Object> menus = (List<Object>)menuSymbol.getValue (new LispList ());
	for (final Object ms : menus)
	{
	    final List<Object> menuSpec = (List<Object>)ms;
	    final String menuName = (String)menuSpec.get (0);
	    final JMenu menu = new JMenu (menuName);
	    for (final Object mis : menuSpec.subList (1, menuSpec.size ()))
	    {
		final JMenuItem item;
		if (mis instanceof List)
		{
		    final List<Object> menuItemSpec = (List<Object>)mis;
		    final String menuItemName = (String)menuItemSpec.get (0);
		    final Object action = menuItemSpec.get (1);
		    item = new JMenuItem (menuItemName);
		    item.addActionListener (new ActionListener ()
		    {
			@Override
			public void actionPerformed (final ActionEvent e)
			{
			    try
			    {
				// Evaluate menu item action
				final LexicalContext context = new LexicalContext (interpreter);
				context.eval (action);
			    }
			    catch (final Exception e1)
			    {
				VerifyPrimitives.incrementReplErrorCount ("Action error " + e1);
				e1.printStackTrace ();
			    }
			}
		    });
		}
		else
		{
		    item = new JMenuItem ((String)mis);
		}
		menu.add (item);
	    }
	    menubar.add (menu);
	}
	frame.setJMenuBar (menubar);
    }
}
