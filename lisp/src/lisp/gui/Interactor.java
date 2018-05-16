
package lisp.gui;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import lisp.*;
import lisp.Package;
import lisp.eval.Interpreter;

/**
 * Swing window for lisp interactions.
 *
 * @author cre
 */
public class Interactor extends JTextPane implements DocumentListener, Runnable
{
    // High priority:
    // [TODO] Not get confused if text before current entry point is deleted
    // [TODO] Indent current form on enter key
    // [TODO] Show balancing paren on entry
    // [TODO] Buttons to halt long computations
    // [TODO] User defined buttons bound to forms
    // [done] Set form/value to variables like repl
    // [done] Print error backtrace into the window.
    // [done] User defined menus bound to forms
    // [TODO] Graphic Inspector embedded or as separate window
    // [TODO] A window or split pane that shows variables (or forms) and values updated each cycle
    // [TODO] Hyperlinks in Interactor output
    // [done] Make System.out print to the interactor

    // Low priority:
    // [TODO] Save/reload window size & position
    // [TODO] Menu or lisp function to control styles.
    // [TODO] Bind the Interactor to a Lisp variable
    // [TODO] Save interactor text to a file
    // [TODO] Find/Replace window
    // [TODO] File editor window for source code
    // [TODO] Help system with hypertext for all functions
    // [TODO] Make this an RCP split window type application?
    // [done] Make prompt into a variable
    // [done] Styles should have different fonts/styles

    private static final MutableAttributeSet BOLD = new SimpleAttributeSet ();
    private static final MutableAttributeSet ITALIC = new SimpleAttributeSet ();

    static
    {
	StyleConstants.setBold (BOLD, true);
	StyleConstants.setItalic (ITALIC, true);
    }

    private static final String PROMPT = ": ";

    private final String[] initString =
	{"Welcome to Jisp!"};

    private final Style rootStyle;
    private final Style noticeStyle;
    private final Style promptStyle;
    private final Style normalStyle;
    private final Style errorStyle;
    private final Style outputStyle;

    private final StyledDocument doc;
    private int position = 0;
    private boolean readInput = false;

    private final LispReader reader;

    private final Interpreter interpreter;
    private final Thread thread = new Thread (this);
    private final Queue<Object> queue = new ConcurrentLinkedDeque<Object> ();

    public Interactor ()
    {
	setBackground (Color.lightGray);
	doc = getStyledDocument ();
	rootStyle = doc.addStyle ("root", null);
	StyleConstants.setFontSize (rootStyle, 12);
	noticeStyle = doc.addStyle ("notice", rootStyle);
	promptStyle = doc.addStyle ("prompt", rootStyle);
	normalStyle = doc.addStyle ("normal", rootStyle);
	errorStyle = doc.addStyle ("error", rootStyle);
	outputStyle = doc.addStyle ("output", rootStyle);
	noticeStyle.addAttributes (BOLD);
	StyleConstants.setForeground (noticeStyle, Color.black);
	StyleConstants.setBackground (noticeStyle, Color.cyan);
	StyleConstants.setFontFamily (noticeStyle, "Monospaced");
	StyleConstants.setFontSize (noticeStyle, 24);
	promptStyle.addAttributes (BOLD);
	outputStyle.addAttributes (ITALIC);
	errorStyle.addAttributes (BOLD);
	for (int i = 0; i < initString.length; i++)
	{
	    log (noticeStyle, initString[i]);
	}
	log (promptStyle, "%n%n");

	position = doc.getLength ();
	setCaretPosition (position);
	doc.addDocumentListener (this);
	interpreter = new Interpreter ();
	reader = new LispReader ();

	// Bind stdout and stderr after constructing the Interpreter so reads from loading the init
	// file are not echoed to the interactor.
	System.setOut (new PrintStream (new InteractorOutputStream (normalStyle)));
	System.setErr (new PrintStream (new InteractorOutputStream (errorStyle)));

	thread.start ();
    }

    public JFrame open (final String title)
    {
	return open (title, true);
    }

    public JFrame open (final String title, final boolean visible)
    {
	final JFrame frame = new JFrame (title);
	frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);

	// Put the editor pane in a scroll pane.
	final JScrollPane scrollPane = new JScrollPane (this);
	scrollPane.setVerticalScrollBarPolicy (JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	scrollPane.setPreferredSize (new Dimension (700, 400));
	scrollPane.setMinimumSize (new Dimension (100, 50));
	addMenubar (frame);
	frame.setContentPane (scrollPane);
	frame.pack ();
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
	final List<Object> menus = (List<Object>)Symbol.value ("user:::*menus*");
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
				interpreter.eval (action);
			    }
			    catch (final Exception e1)
			    {
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
	final JMenu menu1 = new JMenu ("Colour");
	menubar.add (menu1);
	final JMenu menu2 = new JMenu ("Size");
	menubar.add (menu2);

	frame.setJMenuBar (menubar);
    }

    @Override
    public void insertUpdate (final DocumentEvent e)
    {
	tryForm ();
    }

    @Override
    public void removeUpdate (final DocumentEvent e)
    {
	tryForm ();
    }

    @Override
    public void changedUpdate (final DocumentEvent e)
    {
	tryForm ();
    }

    private void tryForm ()
    {
	try
	{
	    if (readInput)
	    {
		final Package pkg = PackageFactory.getDefaultPackage ();
		final String change = doc.getText (position, doc.getLength () - position);
		// System.out.printf ("'%s'%n", change);
		final LispStream stream = new LispStream (change);
		stream.setEofThrows (true);
		final Object form = reader.read (stream, pkg);
		if (form != null)
		{
		    queue.add (form);
		    thread.interrupt ();
		}
	    }
	}
	catch (final EOFException e)
	{
	    // System.out.printf ("EOF%n", e);
	}
	catch (final Throwable e)
	{
	    // System.out.printf ("TryForm Error %s%n", e);
	    logError (e);
	}
    }

    public void run ()
    {
	while (true)
	{
	    final Package pkg = PackageFactory.getDefaultPackage ();
	    final Symbol exprSymbol = pkg.internPrivate ("e").gensym ();
	    log (promptStyle, "[%s] %s", exprSymbol, PROMPT);
	    position = doc.getLength ();
	    setCaretPosition (position);
	    readInput = true;
	    try
	    {
		Object form = null;
		while (form == null)
		{
		    try
		    {
			Thread.sleep (5000);
		    }
		    catch (final InterruptedException e)
		    {
		    }
		    form = queue.poll ();
		}
		if (form != null)
		{
		    readInput = false;
		    exprSymbol.setValue (form);
		    log (outputStyle, "%n");
		    final Object value = interpreter.eval (form);
		    if (value == null)
		    {
			log (outputStyle, "=>null%n");
		    }
		    else
		    {
			final Symbol valueSymbol = pkg.internPrivate ("v").gensym ();
			final String valueText = value.toString ();
			// [TODO] Scan the valueText for format markup and hyperlinks.
			log (outputStyle, "%n[%s] =>%s%n", valueSymbol, valueText);
			valueSymbol.setValue (value);
		    }
		}
	    }
	    catch (final java.lang.reflect.InvocationTargetException e)
	    {
		final Throwable cause = e.getCause ();

		// System.out.printf ("Eval Error %s%n", cause);
		logError (cause);
	    }
	    catch (final Throwable e)
	    {
		// System.out.printf ("Eval Error %s%n", e);
		logError (e);
	    }
	    finally
	    {
		position = doc.getLength ();
		setCaretPosition (position);
		readInput = true;
	    }
	}
    }

    private void log (final Style style, final String format, final Object... args)
    {
	try
	{
	    readInput = false;
	    final String message = String.format (format, args);
	    doc.insertString (doc.getLength (), message, style);
	}
	catch (final BadLocationException e)
	{
	    e.printStackTrace ();
	}
	finally
	{
	    position = doc.getLength ();
	    setCaretPosition (position);
	    readInput = true;
	}
    }

    private void logError (final Throwable e)
    {
	log (errorStyle, "%n%s%n", e.getMessage ());
	final StackTraceElement[] frames = e.getStackTrace ();
	for (final StackTraceElement frame : frames)
	{
	    final String className = frame.getClassName ();
	    final String methodName = frame.getMethodName ();
	    final String fileName = frame.getFileName ();
	    final int lineNumber = frame.getLineNumber ();
	    log (errorStyle, "   at %s.%s(%s:%s)%n", className, methodName, fileName, lineNumber);
	}
    }

    // protected PrintStream outputFile (final String name)
    // {
    // return new PrintStream (new BufferedOutputStream (new FileOutputStream (name)), true);
    // }

    /** A private stream that sends characters to the interactor window. */
    private class InteractorOutputStream extends OutputStream
    {
	private final Style style;

	private InteractorOutputStream (final Style style)
	{
	    this.style = style;
	}

	@Override
	public void write (final int b) throws IOException
	{
	    try
	    {
		readInput = false;
		doc.insertString (doc.getLength (), String.valueOf ((char)b), style);
	    }
	    catch (final BadLocationException e)
	    {
		e.printStackTrace ();
	    }
	    finally
	    {
		position = doc.getLength ();
		setCaretPosition (position);
		readInput = true;
	    }
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }

    public static void main (final String[] args)
    {
	try
	{
	    final Interactor interactor = new Interactor ();
	    interactor.open ("demo");
	}
	catch (final Exception e)
	{
	    e.printStackTrace ();
	}
    }
}
