
package lisp;

import java.io.IOException;

/** Skip blanks and comments from an input stream. */
public class CommentReader
{
    private BlankState state = BlankState.S_NORMAL;

    private enum BlankState
    {
        // Normal State
	S_NORMAL,
        // Line Comment
	S_LINC,
        // Inline comment
	S_INC,
	S_STAR
    }

    /**
     * Skip blanks and comments from an input stream. Implemented using a fast, simple finite state
     * parser.
     */
    public void skipBlanks (final LispStream in) throws IOException
    {
	state = BlankState.S_NORMAL;
	while (!in.eof ())
	{
	    final char chr = in.peek ();
	    switch (state)
	    {
		case S_NORMAL:
		{
		    if (Character.isWhitespace (chr))
		    {
			in.read ();
		    }
		    else if (chr == ';')
		    {
			in.read ();
			state = BlankState.S_LINC;
		    }
		    else if (chr == '/')
		    {
			if (in.peek2 ('*'))
			{
			    state = BlankState.S_INC;
			}
			else
			{
			    // Slash is not a comment. Let someone else handle it.
			    return;
			}
		    }
		    else
		    {
			return;
		    }
		    break;
		}
		case S_LINC:
		    if (chr == '\n')
		    {
			state = BlankState.S_NORMAL;
		    }
		    in.read ();
		    break;
		case S_INC:
		    if (chr == '*')
		    {
			state = BlankState.S_STAR;
		    }
		    in.read ();
		    break;
		case S_STAR:
		    if (chr == '/')
		    {
			state = BlankState.S_NORMAL;
		    }
		    else if (chr != '*')
		    {
			state = BlankState.S_INC;
		    }
		    in.read ();
		    break;
		default:
		    break;
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
