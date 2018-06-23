
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import lisp.lang.*;
import lisp.lang.Package;

class TestLispReader
{
    private final LispReader lispReader = new LispReader ();

    @Test
    void testImportSymbol ()
    {
	final Package system = PackageFactory.findPackage ("lisp.lang");
	assertNotNull (system);
	final Symbol t = system.findSymbol ("true");
	assertNotNull (t);
	lispReader.addImport (t);
	lispReader.removeImport (t);
    }

    @Test
    void testImportPackage ()
    {
	final Package system = PackageFactory.findPackage ("lisp.lang");
	assertNotNull (system);
	lispReader.addImport (system);
	lispReader.removeImport (system);
    }

    @Test
    void testImportClass ()
    {
	lispReader.addImport (System.class);
	lispReader.removeImport (System.class);
    }

    @Test
    void testImportLispPackage ()
    {
	final java.lang.Package lang = java.lang.Package.getPackage ("java.lang");
	assertNotNull (lang);
	lispReader.addImport (lang);
	lispReader.removeImport (lang);
    }

    @Test
    void testImports ()
    {
	assertNotNull (lispReader.getImports ());
    }

    @Test
    void testPeekEOF () throws IOException
    {
	final LispStream stream = new LispInputStream ("a");
	stream.setEofThrows (false);
	assertTrue (stream.peek ('a'));
	assertTrue (stream.peek ('a'));
	assertTrue (stream.tryChar ('a'));
	assertFalse (stream.peek ('a'));
    }

    @Test
    void testPeekEOF2 () throws IOException
    {
	final LispStream stream = new LispInputStream ("a");
	stream.setEofThrows (false);
	assertTrue (stream.peek ('a'));
	assertTrue (stream.peek ('a'));
	assertTrue (stream.tryChar ('a'));
	assertFalse (stream.peek ('a'));

	assertThrows (java.lang.IllegalArgumentException.class, new Executable ()
	{
	    @Override
	    public void execute () throws Throwable
	    {
		stream.read ('a');
	    }
	});
    }

    @Test
    void testSetEOF () throws IOException
    {
	final LispStream stream = new LispStream ()
	{

	    @Override
	    public boolean eof ()
	    {
		return false;
	    }

	    @Override
	    public void close () throws IOException
	    {
	    }

	    @Override
	    public char peek () throws IOException
	    {
		return 0;
	    }

	    @Override
	    public boolean peek (final char expected) throws IOException
	    {
		return false;
	    }

	    @Override
	    public boolean peek2 (final char expected) throws IOException
	    {
		return false;
	    }

	    @Override
	    public char read () throws IOException
	    {
		return 0;
	    }

	    @Override
	    public void read (final char expected) throws IOException
	    {
	    }

	    @Override
	    public boolean tryChar (final char expected) throws IOException
	    {
		return false;
	    }
	};
	stream.setEofThrows (false);
	assertFalse (stream.eof ());
	stream.close ();
	assertEquals (0, stream.peek ());
	assertFalse (stream.peek ('x'));
	assertFalse (stream.peek2 ('x'));
	assertEquals (0, stream.read ());
	stream.read ('x');
	assertFalse (stream.tryChar ('x'));
    }
}
