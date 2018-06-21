
package lisp.cc;

import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import lisp.Symbol;
import lisp.demo.Repl;

class CompilerTest
{
    private void checkVerifyStatus ()
    {
	// assertEquals (0, VerifyPrimitives.getReplErrorCount ());
	for (final String err : VerifyPrimitives.getVerifyErrors ())
	{
	    fail (err.toString ());
	}
	for (final Symbol failure : VerifyPrimitives.getVerifyFailures ())
	{
	    fail (failure.toString ());
	}
    }

    @Test
    void testV4 ()
    {
	final String[] args =
	    {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V4\"", "-l", "Verify.jisp", "--exit", "done test"};
	System.out.println ("Starting testV4");
	Repl.main (args);
	checkVerifyStatus ();
	System.out.println ("Completed testV4");
    }

    @Test
    void testV3 ()
    {
	final String[] args =
	    {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V3\"", "-l", "Verify.jisp", "--exit", "done test"};
	System.out.println ("Starting testV3");
	Repl.main (args);
	checkVerifyStatus ();
	System.out.println ("Completed testV3");
    }

    @Test
    void testConvertV4 ()
    {
	final String[] args =
	    {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V4\"", "-l", "VerifyConvert.jisp", "--exit", "done test"};
	System.out.println ("Starting testConvertV4");
	Repl.main (args);
	checkVerifyStatus ();
	System.out.println ("Completed testConvertV4");
    }

    @Test
    void testConvertV3 ()
    {
	final String[] args =
	    {"-l", "init.jisp", "--setq", "system.compilerVersion=\"V3\"", "-l", "VerifyConvert.jisp", "--exit", "done test"};
	System.out.println ("Starting testConvertV3");
	Repl.main (args);
	checkVerifyStatus ();
	System.out.println ("Completed testConvertV3");
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
