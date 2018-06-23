
package lisp.gui;

import javax.swing.text.Position;

public class HyperLink
{
    private final InteractorEval interactor;
    private final Position startPosition;
    private final Position endPosition;
    private final Object form;
    private boolean hilight = false;

    public HyperLink (final InteractorEval interactor, final Position startPosition, final Position endPosition,
            final Object form)
    {
	this.interactor = interactor;
	this.startPosition = startPosition;
	this.endPosition = endPosition;
	this.form = form;
    }

    // public HyperLink (final StyledDocument doc, final int startPos, final int endPos) throws
    // BadLocationException
    // {
    // this.doc = doc;
    // startPosition = doc.createPosition (startPos);
    // endPosition = doc.createPosition (endPos);
    // }

    public boolean contains (final int pos)
    {
	return startPosition.getOffset () <= pos && pos < endPosition.getOffset ();
    }

    public boolean click (final int pos)
    {
	if (startPosition.getOffset () <= pos && pos < endPosition.getOffset ())
	{
	    interactor.evaluateLink (form);
	    return true;
	}
	return false;
    }

    public int getOffset ()
    {
	return startPosition.getOffset ();
    }

    public int getLength ()
    {
	return endPosition.getOffset () - startPosition.getOffset ();
    }

    public boolean isHilight ()
    {
	return hilight;
    }

    public void setHilight (final boolean hilight)
    {
	this.hilight = hilight;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	final int p1 = startPosition.getOffset ();
	final int p2 = endPosition.getOffset ();
	buffer.append (p1);
	buffer.append (" ");
	buffer.append (p2);
	buffer.append (" ");
	buffer.append (form);
	buffer.append (">");
	return buffer.toString ();
    }
}
