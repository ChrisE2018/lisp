
package lisp.cc;

import java.io.*;
import java.util.logging.Logger;

import lisp.eval.*;
import lisp.lang.*;
import lisp.util.LogString;

// (%defclass foobar (access public) (extends java.lang.Object) (field foo (access public) (type java.lang.Integer) (value 0)))
public class DefclassPrimitives extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (DefclassPrimitives.class.getName ());

    /**
     * Primitive function to define a class. This is intended to be a primitive special form version
     * that provides no syntactic help but works. This only provides quoting and calls the
     * defineClass method. This special form only exists for testing, the defclassMethod itself
     * should be used by most code.
     * <p>
     * Another defclass special form (no quotes) should be defined with better syntax. That should
     * be a macro that produces a call to defineForm.
     * </p>
     *
     * @param context
     * @param members
     * @return
     */
    @DefineLisp (name = "%defclass", special = true)
    // (%defclass <name> <clauses> (extends java.lang.Object) (field public java.lang.Integer foo))
    // (%defclass foobar (access public) (extends java.lang.Object) (field foo (access public) (type
    // java.lang.Integer) (value 0)))
    public Class<?> pctDefclassForm (final LexicalContext context, final Symbol name, final Object... clauses)
    {
	final LispList[] cc = new LispList[clauses.length];
	for (int i = 0; i < clauses.length; i++)
	{
	    cc[i] = (LispList)clauses[i];
	}
	return defineClass (name.getName (), cc);
    }

    @DefineLisp (name = "%defineClass")
    public Class<?> defineClass (final String name, final LispList... clauses)
    {
	try
	{
	    final LispQuotedClassLoader classLoader = new LispQuotedClassLoader ();
	    // FIXME Make this work. The objects for #<ArithmeticPrimitives> need to be loaded.
	    final QuotedData quotedDataHandler = new QuotedDataReader ();
	    final Defclass defclass = new Defclass (quotedDataHandler, name, clauses);
	    final byte[] b = defclass.getBytecode ();
	    final String classBinaryName = null; // Must be null or TraceClassVisitor fails
	    final Class<?> c = classLoader.defineClass (classBinaryName, b);
	    final String fullname = c.getName ();
	    LOGGER.info (new LogString ("Compiled %s as %s", fullname, c));
	    Defclass.forName (fullname, c);
	    return c;
	}
	catch (final Exception e)
	{
	    VerifyPrimitives.incrementReplErrorCount (e.toString ());
	    e.printStackTrace ();
	}
	return null;
    }

    // FIXME (compile pathname &key String:bindir boolean:ifneeded)

    @DefineLisp (name = "%compile", special = true)
    public String pctCompileclassForm (@SuppressWarnings ("unused") final LexicalContext context, final String pathname,
            final String pkgName, final Symbol name, final Object... clauses)
    {
	final LispList[] cc = new LispList[clauses.length];
	for (int i = 0; i < clauses.length; i++)
	{
	    cc[i] = (LispList)clauses[i];
	}
	return compileClass (pathname, pkgName, name.getName (), cc);
    }

    @DefineLisp (name = "%compileClass")
    public String compileClass (final String pathname, final String pkgName, final String name, final LispList... clauses)
    {
	if (pathname.indexOf ('.') < 0)
	{
	    return compileClass (pathname + ".class", pkgName, name, clauses);
	}
	try
	{
	    // FIXME Must use another kind of QuotedData here.
	    final QuotedData quotedDataHandler = new QuotedDataReader ();
	    final Defclass defclass = new Defclass (quotedDataHandler, name, clauses);
	    final byte[] b = defclass.getBytecode ();
	    final File file = new File (pathname);
	    ensureOutputExists (file);
	    final OutputStream stream = new BufferedOutputStream (new FileOutputStream (file));
	    stream.write (b, 0, b.length);
	    stream.flush ();
	    stream.close ();
	    return name;
	}
	catch (final Exception e)
	{
	    VerifyPrimitives.incrementReplErrorCount (e.toString ());
	    e.printStackTrace ();
	}
	return null;
    }

    private void ensureOutputExists (final File file)
    {
	final File folder = file.getParentFile ();
	if (!folder.exists ())
	{
	    folder.mkdirs ();
	}
    }
}
