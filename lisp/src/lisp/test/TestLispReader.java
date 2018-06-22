
package lisp.test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import lisp.lang.*;
import lisp.lang.Package;

class TestLispReader
{
    private final LispReader lispReader = new LispReader ();

    @Test
    void testImportSymbol ()
    {
	final Package system = PackageFactory.findPackage ("system");
	assertNotNull (system);
	final Symbol t = system.findSymbol ("true");
	assertNotNull (t);
	lispReader.addImport (t);
	lispReader.removeImport (t);
    }

    @Test
    void testImportPackage ()
    {
	final Package system = PackageFactory.findPackage ("system");
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
}
