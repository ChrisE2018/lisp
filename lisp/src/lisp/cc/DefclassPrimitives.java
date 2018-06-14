
package lisp.cc;

import java.lang.reflect.*;
import java.util.List;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;

import lisp.LispList;
import lisp.Symbol;
import lisp.eval.*;
import lisp.util.LogString;

// (%defclass foobar (access public) (extends java.lang.Object) (field foo (access public) (type java.lang.Integer) (value 0)))
public class DefclassPrimitives extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (DefclassPrimitives.class.getName ());

    // (package p) (import q)
    // (defclass [public/static/final] foo (extends x) (implements y) (generics) (annotations) .
    // members)
    // TODO Annotations?
    //
    // Members:
    // <fieldDeclaration>
    // <constructorDeclaration>
    // <methodDeclaration>
    // <nestedClass>
    // <staticCode>
    //
    // fieldDeclaration:
    // ([public/private] [static] [type][fieldname] <initform>)
    //
    // constructorDeclaration:
    //
    // methodDeclaration:
    //
    // nestedClass:
    //
    // staticCode:
    //
    // Annotations
    private final int api = Opcodes.ASM5;

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
	    final int asmVersion = Opcodes.V1_5;
	    // The class access, class name, superclass and interfaces need to be determined before
	    // we can visit the class node
	    final LispClassLoader classLoader = new LispClassLoader ();
	    final Defclass defclass = new Defclass (api, classLoader, name, clauses);
	    final int accessCode = defclass.getClassAccess ();
	    final String classSimpleName = defclass.getClassSimpleName ();
	    final Type classType = defclass.getClassType ();
	    final Type superClassType = defclass.getSuperclassType ();
	    final String superClassInternalName = superClassType.getInternalName ();
	    LOGGER.info (new LogString ("Defclass %s extends %s", classSimpleName, superClassType.getClassName ()));
	    final String classInternalName = classType.getInternalName ();
	    final String classBinaryName = classType.getClassName (); // Uses dots
	    final ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);
	    final List<String> interfaceList = defclass.getInterfaces ();
	    final String[] interfaces = new String[interfaceList.size ()];
	    interfaceList.toArray (interfaces);
	    final ClassNode cn = defclass;
	    cn.visit (asmVersion, accessCode, classInternalName, classBinaryName, superClassInternalName, interfaces);
	    // defclass.addDefaultInitMethod ();
	    defclass.addSampleAdditionMethod ();
	    cn.visitEnd ();
	    // optimizers here
	    // cn.accept (new PrintBytecodeClassAdaptor (Compiler.ASM_VERSION, null, new
	    // StringWriter ()));
	    cn.accept (cw);
	    final byte[] b = cw.toByteArray ();
	    final Class<?> c = classLoader.defineClass (classBinaryName, b);
	    LOGGER.info (new LogString ("Compiled %s as %s", classSimpleName, c));
	    return c;
	}
	catch (final Exception e)
	{
	    VerifyPrimitives.incrementReplErrorCount ();
	    e.printStackTrace ();
	}
	return null;
    }

    @DefineLisp
    public void dd (final Class<?> c)
    {
	LOGGER.info (new LogString ("List of Declared Methods"));
	for (final Method method : c.getDeclaredMethods ())
	{
	    LOGGER.info (new LogString ("* Method: %s", method));
	}
	LOGGER.info ("");
    }

    @DefineLisp
    public void tc (final Class<?> c) throws InstantiationException, IllegalAccessException, NoSuchMethodException,
            SecurityException, IllegalArgumentException, InvocationTargetException
    {
	final Object calc = c.newInstance ();
	final Method add = c.getMethod ("add", int.class, int.class);
	System.out.println ("2 + 2 = " + add.invoke (calc, 2, 2));
    }

    @DefineLisp
    public void tc (final Class<?> c, final int a, final int b) throws InstantiationException, IllegalAccessException,
            NoSuchMethodException, SecurityException, IllegalArgumentException, InvocationTargetException
    {
	final Object calc = c.newInstance ();
	final Method add = c.getMethod ("add", int.class, int.class);
	System.out.printf ("%s + %s = %s%n", a, b, add.invoke (calc, a, b));
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
