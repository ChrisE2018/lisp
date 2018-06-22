
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import lisp.lang.*;
import lisp.util.SimpleBiMap;

/**
 * Simple tests of toString methods to increase code coverage. These tests are not important, but
 * make it easier to focus on important code that is not covered by existing tests.
 */
class ToStringTest
{
    private final BraceList braceList = new BraceList ();
    private final BracketList bracketList = new BracketList ();
    private final CommentReader commentReader = new CommentReader ();
    // Describer is an interface
    private final FileReader fileReader = new FileReader ();
    private final LispStream lispStream = new LispInputStream ("(1 2 3)z");
    private final LispReader lispReader = new LispReader ();
    private final Parsing parsing = new Parsing ();

    @Test
    void testBraceList ()
    {
	final String s = braceList.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testBracketList ()
    {
	final String s = bracketList.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testCommentReader ()
    {
	final String s = commentReader.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testFileReader ()
    {
	final String s = fileReader.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testLispLispStream ()
    {
	final String s = lispStream.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testLispLispStream2 () throws IOException
    {
	lispReader.read (lispStream);
	lispStream.setEofThrows (false);
	assertTrue (lispStream.tryChar ('z'));
	assertFalse (lispStream.tryChar ('x'));
	final String s = lispStream.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testLispReader ()
    {
	final String s = lispReader.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testParsing ()
    {
	final String s = parsing.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
    }

    @Test
    void testBiMap ()
    {
	final String[][] values = {{"a", "b"}};
	final SimpleBiMap<String, String> bm = new SimpleBiMap<String, String> (values);
	assertNotNull (bm);
	final String s = bm.toString ();
	assertNotNull (s);
	assertNotEquals (0, s.length ());
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
