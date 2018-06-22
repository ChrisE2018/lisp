
package lisp.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;
import java.util.Map.Entry;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import lisp.lang.Package;
import lisp.lang.PackageFactory;

class TestPackageFactory
{
    @Test
    void testPackageMissing ()
    {
	assertThrows (java.lang.IllegalArgumentException.class, new Executable ()
	{
	    @Override
	    public void execute () throws Throwable
	    {
		PackageFactory.getPackage ("IamNotHere");
	    }
	});
    }

    @Test
    void testPackageMissing2 ()
    {
	assertThrows (java.lang.IllegalArgumentException.class, new Executable ()
	{
	    @Override
	    public void execute () throws Throwable
	    {
		PackageFactory.getPackage ("IamNotHere", false);
	    }
	});
    }

    @Test
    void testPackageCreate ()
    {
	final Package p = PackageFactory.getPackage ("IamNew", true);
	assertNotNull (p);
	assertNotNull (PackageFactory.getPackage ("IamNew", false));
	assertNotNull (PackageFactory.getPackage ("IamNew"));
    }

    @Test
    void testPackageDefault ()
    {
	final Package p = PackageFactory.getPackage ("IamNew");
	assertNotNull (p);
	PackageFactory.setDefaultPackage (p);
	assertEquals (p, PackageFactory.getDefaultPackage ());

	final Package s = PackageFactory.getPackage ("system");
	assertNotNull (s);
	PackageFactory.setDefaultPackage (s);
    }

    @Test
    void testPackageMap ()
    {
	final Map<String, Package> packages = PackageFactory.getPackageMap ();
	for (final Entry<String, Package> entry : packages.entrySet ())
	{
	    final String packageName = entry.getKey ();
	    final Package p = entry.getValue ();
	    assertNotNull (packageName);
	    assertNotNull (p);
	    assertEquals (packageName, p.getName ());
	}
    }
}
