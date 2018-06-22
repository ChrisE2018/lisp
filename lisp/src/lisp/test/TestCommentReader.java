
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import lisp.lang.*;

class TestCommentReader
{
    private final String[][] TEST_CASES = {{"    X", "X"}, {"   y"}, {";\n   z", "z"}, {"/*   */z", "z"}};

    private final String[] EOF_TEST_CASES = {"    ", ";   ", "/*   ", "/", ";", "/*   *"};

    private final CommentReader commentReader = new CommentReader ();

    @Test
    void test () throws IOException
    {
	for (final String[] testCase : TEST_CASES)
	{
	    if (testCase.length == 1)
	    {
		final String data = testCase[0];
		final LispStream stream = new LispInputStream (data);
		commentReader.skipBlanks (stream);
	    }
	    else if (testCase.length == 2)
	    {
		final String data = testCase[0];
		final String expected = testCase[1];
		final LispStream stream = new LispInputStream (data);
		commentReader.skipBlanks (stream);
		for (int i = 0; i < expected.length (); i++)
		{
		    final char e = expected.charAt (i);
		    final char a = stream.read ();
		    assertEquals (e, a);
		}
	    }
	    // else if (testCase.length == 3)
	    // {
	    // final String data = testCase[0];
	    // final String expected = testCase[1];
	    // final String message = testCase[2];
	    // final LispStream stream = new LispInputStream (data);
	    // commentReader.skipBlanks (stream);
	    // for (int i = 0; i < expected.length (); i++)
	    // {
	    // final char e = expected.charAt (i);
	    // final char a = stream.read ();
	    // assertEquals (e, a, message);
	    // }
	    // }
	}
    }

    @Test
    void testEOF ()
    {
	for (final String testCase : EOF_TEST_CASES)
	{
	    final String data = testCase;
	    final LispStream stream = new LispInputStream (data);
	    stream.setEofThrows (true);
	    assertThrows (java.io.EOFException.class, new Executable ()
	    {
		@Override
		public void execute () throws Throwable
		{
		    commentReader.skipBlanks (stream);
		}
	    });
	}
    }

    @Test
    void testEOF2 () throws IOException
    {
	for (final String testCase : EOF_TEST_CASES)
	{
	    final String data = testCase;
	    final LispStream stream = new LispInputStream (data);
	    stream.setEofThrows (false);
	    commentReader.skipBlanks (stream);
	    assertTrue (stream.eof ());
	}
    }

    @Test
    void testThis ()
    {
	final String s = toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
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
