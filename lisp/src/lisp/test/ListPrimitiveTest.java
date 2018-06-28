
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import lisp.cc.VerifyPrimitives;
import lisp.gui.Repl;
import lisp.lang.Symbol;

class ListPrimitiveTest
{
    private static final char DOUBLE_QUOTE = '"';
    private static final String COMPILER_VERSION = "V4";
    private static final String INIT_FILE = "init.jisp";
    private static final String TEST_FILE = "../test/ListTest.jisp";

    @Test
    void testListPrimitivees ()
    {
	final String compilerVersion = "system.compilerVersion=" + quoted (COMPILER_VERSION);
	final String[] args = {"-l", INIT_FILE, "--setq", compilerVersion, "-l", TEST_FILE, "--exit", "done test"};
	Repl.main (args);
	checkVerifyStatus ();
	final int count = VerifyPrimitives.getTotalTestCount ();
	assertTrue (count > 0);
    }

    private void checkVerifyStatus ()
    {
	for (final String err : VerifyPrimitives.getVerifyErrors ())
	{
	    fail (err.toString ());
	}
	for (final Symbol failure : VerifyPrimitives.getVerifyFailures ())
	{
	    fail (failure.toString ());
	}
    }

    private String quoted (final String s)
    {
	return DOUBLE_QUOTE + s + DOUBLE_QUOTE;
    }
}
