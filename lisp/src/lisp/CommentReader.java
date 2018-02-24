
package lisp;

import java.io.*;

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

    public void skipBlanks (final InputStream in) throws IOException
    {
	state = BlankState.S_NORMAL;
	while (true)
	{
	    final char chr = peek (in);
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
			in.mark (2);
			in.read ();
			if ((char)in.read () == '*')
			{
			    state = BlankState.S_INC;
			}
			else
			{
			    // Slash is not a comment. Let someone else handle it.
			    in.reset ();
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

    private char peek (final InputStream in) throws IOException
    {
	in.mark (1);
	final int input = in.read ();
	in.reset ();
	return (char)input;
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
}
