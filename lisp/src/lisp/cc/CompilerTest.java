
package lisp.cc;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.Test;

import lisp.Symbol;
import lisp.demo.Repl;

class CompilerTest
{
    private void checkVerifyStatus ()
    {
	if (VerifyPrimitives.getReplErrorCount () > 0)
	{
	    final List<Symbol> errors = VerifyPrimitives.getVerifyFailures ();
	    final StringBuilder buffer = new StringBuilder ();
	    buffer.append (errors.size ());
	    buffer.append ("failures\n");
	    for (int i = 0; i < errors.size (); i++)
	    {
		buffer.append (i);
		buffer.append (": ");
		buffer.append (errors.get (i));
		buffer.append ("\n");
	    }
	    fail (buffer.toString ());
	}
	else
	{
	    assertEquals (VerifyPrimitives.getReplErrorCount (), 0);
	}
    }

    @Test
    void testV4 ()
    {
	final String[] args = {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V4\"", "-l", "Verify.jisp"};
	Repl.main (args);
	checkVerifyStatus ();
    }

    @Test
    void testV3 ()
    {
	final String[] args = {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V3\"", "-l", "Verify.jisp"};
	Repl.main (args);
	checkVerifyStatus ();
    }

    @Test
    void testConvertV4 ()
    {
	final String[] args = {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V4\"", "-l", "VerifyConvert.jisp"};
	Repl.main (args);
	checkVerifyStatus ();
    }

    @Test
    void testConvertV3 ()
    {
	final String[] args = {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V3\"", "-l", "VerifyConvert.jisp"};
	Repl.main (args);
	checkVerifyStatus ();
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
