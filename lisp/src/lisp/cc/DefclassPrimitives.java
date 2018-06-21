
package lisp.cc;

import java.io.*;
import java.util.List;
import java.util.logging.*;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.ClassNode;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc4.Optimizer;
import lisp.eval.*;
import lisp.util.LogString;

// (%defclass foobar (access public) (extends java.lang.Object) (field foo (access public) (type java.lang.Integer) (value 0)))
public class DefclassPrimitives extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (DefclassPrimitives.class.getName ());

    private final int asmApi = Opcodes.ASM5;
    private final int bytecodeVersion = Opcodes.V1_5;

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
    public Class<?> pctDefclassForm (@SuppressWarnings ("unused") final LexicalContext context, final Symbol name,
            final Object... clauses)
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
	    final String pkgName = "lisp.cc";
	    final LispClassLoader classLoader = new LispClassLoader ();
	    final byte[] b = getDefclassBytecode (classLoader, pkgName, name, clauses);
	    final String classBinaryName = null; // Must be null or TraceClassVisitor fails
	    final Class<?> c = classLoader.defineClass (classBinaryName, b);
	    LOGGER.info (new LogString ("Compiled %s as %s", name, c));
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
	    final LispClassLoader classLoader = new LispClassLoader ();
	    final byte[] b = getDefclassBytecode (classLoader, pkgName, name, clauses);
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

    private byte[] getDefclassBytecode (final LispClassLoader classLoader, final String pkgName, final String name,
            final LispList[] clauses)
    {
	// The class access, class name, superclass and interfaces need to be determined before
	// we can visit the class node
	final Defclass defclass = new Defclass (asmApi, classLoader, pkgName, name, clauses);
	final int accessCode = defclass.getClassAccess ();
	final String classSimpleName = defclass.getClassSimpleName ();
	final Type classType = defclass.getClassType ();
	final Type superClassType = defclass.getSuperclassType ();
	final String superClassInternalName = superClassType.getInternalName ();
	LOGGER.info (new LogString ("Defclass %s extends %s", classSimpleName, superClassType.getClassName ()));
	final String classInternalName = classType.getInternalName ();
	// final String classBinaryName = classType.getClassName (); // Uses dots
	final String classBinaryName = null; // Must be null or TraceClassVisitor fails
	final ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);
	final List<String> interfaceList = defclass.getInterfaces ();
	final String[] interfaces = new String[interfaceList.size ()];
	interfaceList.toArray (interfaces);
	final ClassNode cn = defclass;
	cn.visit (bytecodeVersion, accessCode, classInternalName, classBinaryName, superClassInternalName, interfaces);
	// defclass.addSampleAdditionMethod ();
	cn.visitEnd ();
	// optimizers here
	final Logger pbl = Logger.getLogger (PrintBytecodeClassAdaptor.class.getName ());
	final boolean optimize = Symbol.named ("system", "optimize").getBooleanValue (true);
	final boolean printBytecode = pbl.isLoggable (Level.INFO);

	// Form chain adding things in the middle.
	// The last thing to do is write the code using cw.
	ClassVisitor cv = cw;

	if (printBytecode)
	{
	    // Install PrintBytecodeClassAdaptor before Optimizer so it runs after.
	    // Move this block lower to print code before optimization.
	    cv = new PrintBytecodeClassAdaptor (asmApi, cv, new StringWriter ());
	}
	if (optimize)
	{
	    cv = new Optimizer (Compiler.ASM_VERSION, cv);
	}

	// Put code into init method to initialize fields and quoted data
	// This should be the last operation installed (first done)
	cv = defclass.new InitModifierClassVisitor (cv);
	cn.accept (cv);

	final byte[] b = cw.toByteArray ();
	return b;
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
