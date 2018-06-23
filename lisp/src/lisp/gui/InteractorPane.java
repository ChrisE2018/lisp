
package lisp.gui;

import java.awt.Color;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.ConcurrentLinkedDeque;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import lisp.cc.VerifyPrimitives;
import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;

public class InteractorPane extends JTextPane
        implements DocumentListener, Runnable, MouseListener, MouseMotionListener, InteractorEval
{
    // High priority:
    // [done] Indent current form on enter key
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

    public InteractorPane (final Interpreter interpreter)
    {
	this.interpreter = interpreter;
	reader = new LispReader ();
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
	final int condition = JComponent.WHEN_FOCUSED;
	final InputMap inputMap = getInputMap (condition);
	final ActionMap actionMap = getActionMap ();

	final String enter = "enter";
	inputMap.put (KeyStroke.getKeyStroke (KeyEvent.VK_ENTER, 0), enter);
	actionMap.put (enter, new AbstractAction ()
	{
	    @Override
	    public void actionPerformed (final ActionEvent e)
	    {
		// out.println ("enter pressed");
		try
		{
		    doc.insertString (getCaretPosition (), "\n", normalStyle);
		    // FIXME Make extra indentation a variable
		    final int indent = determineIndetation () + 3;
		    for (int i = 0; i < indent; i++)
		    {
			doc.insertString (getCaretPosition (), " ", normalStyle);
		    }
		}
		catch (final BadLocationException e1)
		{
		    // e1.printStackTrace();
		}
	    }
	});

	// Bind stdout and stderr after constructing the Interpreter so reads from loading the init
	// file are not echoed to the interactor.
	System.setOut (new PrintStream (new InteractorOutputStream (normalStyle)));
	System.setErr (new PrintStream (new InteractorOutputStream (errorStyle)));

	thread.start ();
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

    private int determineIndetation ()
    {
	final Package pkg = PackageFactory.getCurrentPackage ();
	final int offset = inputPosition.getOffset () + 1;
	// final String change = doc.getText (offset, getCaretPosition () - offset);
	// out.printf ("[%d] '%s'%n", offset, change);
	final LispTextPaneStream stream = new LispTextPaneStream (this, offset, getCaretPosition ());
	stream.setEofThrows (true);
	try
	{
	    reader.read (stream, pkg);
	}
	catch (final EOFException e)
	{
	    return stream.getIndentation ();
	}
	catch (final IOException e)
	{
	    e.printStackTrace ();
	}
	return 0;
    }

    private void tryForm ()
    {
	try
	{
	    if (readInput)
	    {
		final Package pkg = PackageFactory.getCurrentPackage ();
		final int offset = inputPosition.getOffset () + 1;
		final String change = doc.getText (offset, doc.getLength () - offset);
		// out.printf ("[%d] '%s'%n", offset, change);
		final LispStream stream = new LispInputStream (change);
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
	    VerifyPrimitives.incrementReplErrorCount ("TryForm Error " + e);
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
		final Package pkg = PackageFactory.getCurrentPackage ();
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
		final Throwable cause = e.getCause ();
		VerifyPrimitives.incrementReplErrorCount ("Eval Error " + cause);

		err.printf ("Eval Error %s%n", cause);
		logError (cause);
	    }
	    catch (final Throwable e)
	    {
		VerifyPrimitives.incrementReplErrorCount ("Eval Error " + e);
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

    public void evaluateLink (final Object form)
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
