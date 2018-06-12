
package lisp.gui;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.logging.LogManager;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import lisp.*;
import lisp.Package;
import lisp.cc.VerifyPrimitives;
import lisp.eval.*;

/**
 * Swing window for lisp interactions.
 *
 * @author cre
 */
public class Interactor extends JTextPane implements DocumentListener, Runnable, MouseListener, MouseMotionListener
{
    // High priority:
    // TODO Indent current form on enter key
    // TODO Show balancing paren on entry
    // [skip] Buttons to halt long computations
    // CONSIDER User defined buttons bound to forms
    // [done] Set form/value to variables like repl
    // [done] Print error backtrace into the window.
    // [done] User defined menus bound to forms
    // [done] Not get confused if text before current entry point is deleted
    // CONSIDER Graphic Inspector embedded or as separate window
    // CONSIDER A window or split pane that shows variables (or forms) and values updated each cycle
    // [done] Hyperlinks in Interactor output
    // TODO Make hyperlinks easier to use in client code and better appearance
    // Note: editable text won't normally process hyperlinks. @see
    // http://java-sl.com/tip_links_in_editable.html
    // [done] Make System.out print to the interactor

    // Low priority:
    // [skip] Save/reload window size & position
    // [skip] Menu or lisp function to control styles.
    // [skip] Bind the Interactor to a Lisp variable
    // CONSIDER Save interactor text to a file
    // [skip] Find/Replace window
    // TODO File editor window for source code
    // [skip] Help system with hypertext for all functions
    // CONSIDER Make this an RCP split window type application?
    // [done] Make prompt into a variable
    // [done] Styles should have different fonts/styles

    // @see https://docs.oracle.com/javase/8/javafx/user-interface-tutorial/editor.htm

    // private static final Logger LOGGER = Logger.getLogger (Interactor.class.getName ());

    // private static final MutableAttributeSet UNBOLD = new SimpleAttributeSet ();
    private static final MutableAttributeSet BOLD = new SimpleAttributeSet ();
    private static final MutableAttributeSet ITALIC = new SimpleAttributeSet ();
    private static final MutableAttributeSet HILITE_ON = new SimpleAttributeSet ();
    private static final MutableAttributeSet HILITE_OFF = new SimpleAttributeSet ();

    static
    {
	StyleConstants.setBold (BOLD, true);
	StyleConstants.setItalic (ITALIC, true);
	StyleConstants.setBold (HILITE_ON, true);
	StyleConstants.setUnderline (HILITE_ON, true);
	StyleConstants.setBold (HILITE_OFF, false);
	StyleConstants.setUnderline (HILITE_OFF, false);
    }

    private static final String PROMPT = ": ";

    private final String[] initString = {"Welcome to Jisp!"};

    private final Style rootStyle;
    private final Style noticeStyle;
    private final Style promptStyle;
    private final Style normalStyle;
    private final Style errorStyle;
    private final Style outputStyle;

    private final StyledDocument doc;

    /**
     * Position of the character before the input form. We record the position before so it will
     * move correctly if characters are added or removed before the start of the input form.
     */
    private Position inputPosition = null;
    private boolean readInput = false;

    private final LispReader reader;

    private final Interpreter interpreter;
    private final Thread thread = new Thread (this);
    private final Queue<Object> queue = new ConcurrentLinkedDeque<Object> ();

    private final PrintStream out = System.out;
    private final PrintStream err = System.err;

    private final List<HyperLink> links = new ArrayList<HyperLink> ();

    public Interactor (final String[] args) throws Exception
    {
	LogManager.getLogManager ()
	        .readConfiguration (Interactor.class.getResource ("loggingBootstrap.properties").openStream ());
	final Application application = new Application ();
	application.initialize (args);
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
	setInputAtEnd ();
	doc.addDocumentListener (this);
	addMouseListener (this);
	addMouseMotionListener (this);
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
				VerifyPrimitives.incrementReplErrorCount ();
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
		final int offset = inputPosition.getOffset () + 1;
		final String change = doc.getText (offset, doc.getLength () - offset);
		// out.printf ("[%d] '%s'%n", offset, change);
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
	    VerifyPrimitives.incrementReplErrorCount ();
	    // System.out.printf ("TryForm Error %s%n", e);
	    logError (e);
	}
    }

    public void run ()
    {
	out.printf ("Starting interactor thread%n");
	while (true)
	{
	    try
	    {
		final Package pkg = PackageFactory.getDefaultPackage ();
		final Symbol exprSymbol = pkg.internSymbol ("e").gensym ();
		final int startPos = doc.getLength ();
		log (promptStyle, "[%s]", exprSymbol);
		final int endPos = doc.getLength ();
		log (promptStyle, " %s", PROMPT);
		links.add (new HyperLink (this, doc.createPosition (startPos), doc.createPosition (endPos), exprSymbol));
		setCaretPosition (doc.getLength ());
		readInput = true;
		// Now get next form
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
		    final LexicalContext context = new LexicalContext (interpreter);
		    final long startTime = System.currentTimeMillis ();
		    final Object value = context.eval (form);
		    final long duration = System.currentTimeMillis () - startTime;
		    if (value == null)
		    {
			log (outputStyle, "=>null%n");
		    }
		    else
		    {
			final Symbol valueSymbol = pkg.internSymbol ("v").gensym ();
			final String valueText = value.toString ();
			// TODO Scan the valueText for format markup and hyperlinks.
			final int p1 = doc.getLength ();
			log (outputStyle, "%n[%s] =>%s", valueSymbol, valueText);
			final int p2 = doc.getLength ();
			log (outputStyle, "%n");
			valueSymbol.setValue (value);
			links.add (new HyperLink (this, doc.createPosition (p1), doc.createPosition (p2), valueSymbol));
		    }
		    log (outputStyle, "%d ms%n", duration);
		}
	    }
	    catch (final java.lang.reflect.InvocationTargetException e)
	    {
		VerifyPrimitives.incrementReplErrorCount ();
		final Throwable cause = e.getCause ();

		err.printf ("Eval Error %s%n", cause);
		logError (cause);
	    }
	    catch (final Throwable e)
	    {
		VerifyPrimitives.incrementReplErrorCount ();
		err.printf ("Eval Error %s%n", e);
		logError (e);
	    }
	    finally
	    {
		setInputAtEnd ();
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
	    setInputAtEnd ();
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
		setInputAtEnd ();
		readInput = true;
	    }
	}
    }

    private void setInputAtEnd ()
    {
	try
	{
	    final int offset = doc.getLength ();
	    setCaretPosition (offset);
	    inputPosition = doc.createPosition (offset - 1);
	}
	catch (final BadLocationException e)
	{
	    e.printStackTrace ();
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

    public static void main (final String[] args)
    {
	final PrintStream err = System.err;
	try
	{
	    final Interactor interactor = new Interactor (args);
	    interactor.open ("Interactor");
	}
	catch (final java.lang.reflect.InvocationTargetException e)
	{
	    Throwable ee = e;
	    for (int i = 0; i < 10 && ee instanceof java.lang.reflect.InvocationTargetException; i++)
	    {
		ee = ee.getCause ();
	    }
	    err.printf ("Initialization error %s %n", ee);
	    ee.printStackTrace (err);
	}
	catch (final Throwable e)
	{
	    err.printf ("Initialization error %s %n", e);
	    e.printStackTrace (err);
	}
    }

    public void eval (final Object form)
    {
	queue.add (form);
	thread.interrupt ();
    }

    @Override
    public void mouseClicked (final MouseEvent e)
    {
	final int pos = viewToModel (e.getPoint ());
	for (final HyperLink link : links)
	{
	    // out.printf ("Checking link %s %n", link);
	    if (link.click (pos))
	    {
		out.printf ("Mouse in %s at %s %n", link, pos);
		return;
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

    @Override
    public void mouseDragged (final MouseEvent e)
    {
    }

    @Override
    public void mouseMoved (final MouseEvent e)
    {
	final int pos = viewToModel (e.getPoint ());
	for (final HyperLink link : links)
	{
	    // out.printf ("Checking link %s %n", link);
	    if (link.contains (pos))
	    {
		if (!link.isHilight ())
		{
		    // out.printf ("Mouse in %s at %s %n", link, pos);
		    doc.setParagraphAttributes (link.getOffset (), link.getLength (), HILITE_ON, false);
		    link.setHilight (true);
		}
	    }
	    else if (link.isHilight ())
	    {
		doc.setParagraphAttributes (link.getOffset (), link.getLength (), HILITE_OFF, false);
		link.setHilight (false);
	    }
	}
    }
}
