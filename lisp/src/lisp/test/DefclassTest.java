
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.logging.LogManager;

import org.junit.jupiter.api.*;

import lisp.cc.Defclass;
import lisp.eval.*;
import lisp.gui.Interactor;
import lisp.lang.*;
import lisp.lang.FileReader;
import lisp.primitives.LoggerPrimitives;

class DefclassTest
{
    private static final LogManager logManager = LogManager.getLogManager ();
    private static final LoggerPrimitives loggerPrimitives = new LoggerPrimitives ();
    private final Interpreter interpreter = new Interpreter ();
    private final LexicalContext context = new LexicalContext (interpreter);

    @BeforeAll
    static void setUpBeforeClass () throws Exception
    {
	logManager.readConfiguration (Interactor.class.getResource ("loggingBootstrap.properties").openStream ());

	final FileReader fileReader = new FileReader ();
	final URL url = Definer.class.getResource ("init.jisp");
	assertTrue (fileReader.read (new LexicalContext (new Interpreter ()), url));
	// -l init.jisp
	// -E " (showBytecode f) "
	// --setq 'system.compilerVersion="V4"'
	loggerPrimitives.showBytecode (false);
	Symbol.named ("lisp.lang", "compilerVersion").setValue ("V4");
	PackageFactory.getPackage ("lisp.test", true);
	assertNotNull (PackageFactory.getPackage ("lisp.test"));
    }

    @Test
    void checkFile () throws Exception
    {
	final URL url = getClass ().getResource ("Sample.jisp");
	final InputStream in = url.openStream ();
	in.close ();
    }

    @Test
    void checkFile2 () throws Exception
    {
	final URL url = getClass ().getResource ("Sample.jisp");
	final LispReader lispReader = new LispReader ();
	try (InputStream in = url.openStream ())
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispInputStream (b);
	    final Object form = lispReader.read (stream);
	    assertNotNull (form);
	    System.out.println (form);
	}
    }

    @Test
    void checkFile3 () throws Exception
    {
	final URL url = getClass ().getResource ("Simple.jisp");
	final LispReader lispReader = new LispReader ();
	try (InputStream in = url.openStream ())
	{
	    final BufferedInputStream b = new BufferedInputStream (in);
	    final LispStream stream = new LispInputStream (b);
	    final Object form = lispReader.read (stream);
	    assertNotNull (form);
	    System.out.println (form);
	}
    }

    @Test
    void testSimple () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Simple.jisp");
	assertTrue (fileReader.read (context, url));
    }

    @Test
    void testSimpleClass () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Simple.jisp");
	assertTrue (fileReader.read (context, url));
	@SuppressWarnings ("unchecked")
	final Class<? extends SimpleInterface> cls = (Class<? extends SimpleInterface>)Defclass.forName ("lisp.test.Simple");
	assertNotNull (cls);
	final SimpleInterface s = cls.newInstance ();
	assertEquals (5, s.getBlahX ());
	final Symbol alpha = PackageFactory.getPackage ("lisp.test").internSymbol ("alpha");
	alpha.setValue (54);
	assertEquals (3 + 4 + 5 + 54, s.foo ()); // (+ 3 4 blah int:alpha)
	assertEquals ("#<Simple 5>", s.toString ());
    }

    @Test
    void test () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Sample.jisp");
	assertTrue (fileReader.read (context, url));
    }

    @Test
    void testSampleClass () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Sample.jisp");
	assertTrue (fileReader.read (context, url));
	final Class<?> cls = Defclass.forName ("lisp.test.Sample");
	assertNotNull (cls);
	if (cls != null)
	{
	    final Object s = cls.newInstance ();
	    assertTrue (s instanceof SampleInterface);
	    final SampleInterface si = (SampleInterface)s;
	    assertEquals (3, si.getBlahX ());
	    assertEquals (7, si.foo ());
	    assertEquals (99, si.bar (99));
	    assertEquals (99, si.getBlahX ());
	}
    }

    @Test
    void testIntConstructor () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Sample.jisp");
	assertTrue (fileReader.read (context, url));
	@SuppressWarnings ("unchecked")
	final Class<? extends SampleInterface> cls = (Class<? extends SampleInterface>)Defclass.forName ("lisp.test.Sample");
	assertNotNull (cls);
	final Constructor<? extends SampleInterface> constructor = cls.getConstructor (int.class);
	final SampleInterface s = constructor.newInstance (67);
	assertEquals (67, s.getBlahX ());
	assertEquals ("#<Sample 67>", s.toString ());
    }

    @Test
    void testDoubleConstructor () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource ("Sample.jisp");
	assertTrue (fileReader.read (context, url));
	@SuppressWarnings ("unchecked")
	final Class<? extends SampleInterface> cls = (Class<? extends SampleInterface>)Defclass.forName ("lisp.test.Sample");
	assertNotNull (cls);
	final Constructor<? extends SampleInterface> constructor = cls.getConstructor (double.class);
	final SampleInterface s = constructor.newInstance (67.0);
	assertEquals (6, s.getBlahX ());
    }
}
