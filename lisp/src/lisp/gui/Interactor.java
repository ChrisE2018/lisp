
package lisp.gui;

import java.awt.*;
import java.awt.event.*;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.logging.LogManager;

import javax.swing.*;

import lisp.cc.VerifyPrimitives;
import lisp.eval.*;
import lisp.lang.*;

public class Interactor
{
    private static final String WINDOW_NAME = "Interactor";
    private static final String[] DEFAULT_ARGS = {"-g", "logging.properties", "-l", "init.jisp"};
    private final Interpreter interpreter;
    private final InteractorPane interactor;
    private final PrintStream err = System.err;

    public static void main (final String[] args)
    {
	final PrintStream err = System.err;
	try
	{
	    final Interactor app = new Interactor (args.length > 0 ? args : DEFAULT_ARGS);
	    SwingUtilities.invokeLater (new Runnable ()
	    {
		@Override
		public void run ()
		{
		    app.open (WINDOW_NAME, true);
		}
	    });
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

    private Interactor (final String[] args) throws Exception
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

	final JPanel content = new JPanel ();
	content.setLayout (new BorderLayout ());
	// Put the editor pane in a scroll pane.
	final JScrollPane scrollPane = new JScrollPane (interactor);
	scrollPane.setVerticalScrollBarPolicy (JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	scrollPane.setPreferredSize (new Dimension (700, 400));
	scrollPane.setMinimumSize (new Dimension (100, 50));
	content.add (scrollPane, BorderLayout.NORTH);
	content.add (getButtonbar (), BorderLayout.SOUTH);
	frame.setContentPane (content);
	frame.setJMenuBar (getMenubar ());
	frame.pack ();
	frame.setLocationRelativeTo (null);
	if (visible)
	{
	    frame.setVisible (visible);
	}
	return frame;
    }

    @SuppressWarnings ("unchecked")
    private JPanel getButtonbar ()
    {
	final JPanel result = new JPanel ();
	final Symbol buttonSymbol = PackageFactory.getSystemPackage ().internSymbol ("*buttons*");
	final List<Object> buttons = (List<Object>)buttonSymbol.getValue (new LispList ());
	for (final Object spec : buttons)
	{
	    final List<Object> buttonSpec = (List<Object>)spec;
	    final String buttonName = (String)buttonSpec.get (0);
	    final Object buttonAction = buttonSpec.get (1);
	    final JButton button = new JButton (buttonName);

	    button.addActionListener (new ActionListener ()
	    {
		@Override
		public void actionPerformed (final ActionEvent e)
		{
		    try
		    {
			final LexicalContext context = new LexicalContext (interpreter);
			context.eval (buttonAction);
		    }
		    catch (final InvocationTargetException ex)
		    {
			Throwable cause = ex;
			for (int i = 0; i < 10 && cause instanceof InvocationTargetException; i++)
			{
			    cause = cause.getCause ();
			}
			VerifyPrimitives.incrementReplErrorCount ("Action error " + cause);
			cause.printStackTrace (err);
		    }
		    catch (final Throwable ex)
		    {
			VerifyPrimitives.incrementReplErrorCount ("Action error " + ex);
			ex.printStackTrace (err);
		    }
		}
	    });
	    result.add (button);
	}
	return result;
    }

    @SuppressWarnings ("unchecked")
    private JMenuBar getMenubar ()
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
	return menubar;
    }
}
