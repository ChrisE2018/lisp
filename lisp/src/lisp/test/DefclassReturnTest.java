
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.net.URL;
import java.util.logging.LogManager;

import org.junit.jupiter.api.*;

import lisp.cc.Defclass;
import lisp.eval.*;
import lisp.gui.Interactor;
import lisp.lang.*;
import lisp.lang.FileReader;
import lisp.primitives.LoggerPrimitives;

class DefclassReturnTest
{
    private static final LogManager logManager = LogManager.getLogManager ();
    private static final LoggerPrimitives loggerPrimitives = new LoggerPrimitives ();
    private final Interpreter interpreter = new Interpreter ();
    private final LexicalContext context = new LexicalContext (interpreter);

    private static final String LISP_TEST_FILE = "ReturnSample.jisp";

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
	final URL url = getClass ().getResource (LISP_TEST_FILE);
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
    void testSimpleClass () throws Exception
    {
	final FileReader fileReader = new FileReader ();
	final URL url = getClass ().getResource (LISP_TEST_FILE);
	assertTrue (fileReader.read (context, url));
	@SuppressWarnings ("unchecked")
	final Class<? extends SampleInterface> cls = (Class<? extends SampleInterface>)Defclass.forName ("lisp.test.Sample");
	assertNotNull (cls);
	final SampleInterface s = cls.newInstance ();
	assertEquals (3 + 4, s.foo ());
	assertEquals ("#<Sample 33>", s.toString ());
    }
}
