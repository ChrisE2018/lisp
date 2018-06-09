
package lisp.cc;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;
// @see https://www.beyondjava.net/blog/quick-guide-writing-byte-code-asm/
import org.objectweb.asm.util.*;

import lisp.*;
import lisp.Symbol;

public class CompileLoader_v1 extends ClassLoader implements Compiler
{
    private static final Logger LOGGER = Logger.getLogger (CompileLoader_v1.class.getName ());

    /**
     * Predefined shell class structure with support methods. To create a compiled function we load
     * this shell class and inject our new method. Each compile requires a new instance of this
     * ClassLoaded so we get a distinct class as a result. <br/>
     * [TODO] Maybe use a gensym in the defineClass call to allow ClassLoader re-use?
     */
    private static final String SHELL_CLASS = "lisp.cc.CompiledShell";

    /**
     * Flag to control display of bytecode after compile. <br/>
     * [TODO] Should use logger configuration instead of hard-coded flags.
     */
    private static final String SHOW_BYTECODE = "showBytecode";

    /**
     * Trick to compile references to quoted data. This map is obtained by the method compiler and
     * changed by side effect using unique gensym keys. The bytecode uses the gensym Symbol name to
     * locate the quoted data. This works only because we are loading compiled bytecode into the
     * same instance of the JVM that we used to compile the code. If we need to save compiled code
     * to a file and load it later, the quoted structure would have to be reconstructed at load
     * time.
     */
    private final Map<String, Object> quotedReferences = new HashMap<String, Object> ();

    private final Class<?> returnType;

    private final String methodName;

    private final LispList methodArgs;

    private final LispList methodBody;

    public CompileLoader_v1 (final Class<?> returnType, final String methodName, final LispList methodArgs,
            final LispList methodBody)
    {
	this.returnType = returnType;
	this.methodName = methodName;
	this.methodArgs = methodArgs;
	this.methodBody = methodBody;
    }

    /**
     * Load the shell class resource and run the ClassVisitor on it to add our new method.
     */
    public Class<?> compile () throws IOException
    {
	LOGGER.info (String.format ("Creating compiled class: %s for function %s %s %s", SHELL_CLASS, returnType, methodName,
	        methodArgs));
	final String className = SHELL_CLASS;
	LOGGER.info (String.format ("Adding method %s %s to class: %s", methodName, methodArgs, className));
	final String resourceName = className.replace ('.', '/');
	final String resource = resourceName + ".class";
	final InputStream is = getResourceAsStream (resource);
	final ClassReader cr = new ClassReader (is);
	final ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);
	ClassVisitor cv2 = cw;

	final StringWriter sw = new StringWriter ();
	final Symbol showBytecodeSymbol = PackageFactory.getSystemPackage ().internSymbol (SHOW_BYTECODE);
	final boolean showBytecode = showBytecodeSymbol.getValue (false) != Boolean.FALSE;
	if (showBytecode)
	{
	    final Printer printer = new Textifier ();
	    cv2 = new TraceClassVisitor (cw, printer, new PrintWriter (sw));
	}
	final ClassVisitor cv =
	    new CompileClassAdaptor_v1 (cv2, resourceName, returnType, methodName, methodArgs, methodBody, quotedReferences);

	cr.accept (cv, 0);
	if (showBytecode)
	{
	    System.out.println (sw.toString ());
	}
	final byte[] b = cw.toByteArray ();
	final Class<?> c = defineClass (className, b, 0, b.length);
	return c;
    }

    /** Provide access to ASM internals if possible. */
    public ClassNode getClassNode ()
    {
	return null;
    }

    /** Provide access to ASM internals if possible. */
    public MethodNode getMethodNode ()
    {
	return null;
    }

    // private void checkCreatedClass (final Class<?> c) throws InstantiationException,
    // IllegalAccessException,
    // IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException
    // {
    // LOGGER.info (String.format ("List of Declared Methods"));
    // for (final Method method : c.getDeclaredMethods ())
    // {
    // LOGGER.info (String.format ("* Method: %s", method));
    // }
    //
    // // System.out.printf ("Calling newInstance()%n");
    // final Object instance = c.newInstance ();
    // // System.out.printf ("=> Instance: %s %n", instance);
    // // System.out.printf ("%n");
    // checkNewInstance (c, instance);
    //
    // final Class<?>[] types =
    // {int.class};
    // final Constructor<?> con = c.getConstructor (types);
    // // System.out.printf ("Calling newInstance(1)%n");
    // final Object in2 = con.newInstance (1);
    // // System.out.printf ("=> Instance: %s %n", in2);
    // checkNewInstance (c, in2);
    // }

    // private void checkNewInstance (final Class<?> c, final Object instance)
    // throws IllegalAccessException, IllegalArgumentException, InvocationTargetException
    // {
    // System.out.printf ("Calling Zero Param Instance Methods%n");
    // for (final Method method : c.getDeclaredMethods ())
    // {
    // if (Modifier.isPublic (method.getModifiers ()))
    // {
    // final Class<?>[] params = method.getParameterTypes ();
    // if (params.length == 0)
    // {
    // final Object result = method.invoke (instance);
    // if (result == null)
    // {
    // System.out.printf ("* Invoke Method %s() => %s [null] %n", method.getName (), result);
    // }
    // else
    // {
    // System.out.printf ("* Invoke Method %s() => %s %s %n", method.getName (), result,
    // result.getClass ());
    // }
    // }
    // }
    // }
    // System.out.printf ("%n");
    //
    // System.out.printf ("Calling One Param Instance Methods%n");
    // for (final Method method : c.getDeclaredMethods ())
    // {
    // // System.out.printf ("Method: %s %n", method);
    // if (Modifier.isPublic (method.getModifiers ()))
    // {
    // final Class<?>[] params = method.getParameterTypes ();
    // if (params.length == 1)
    // {
    // final String arg = "blah";
    // final Object result = method.invoke (instance, arg);
    // if (result == null)
    // {
    // System.out.printf ("* Invoke Method %s() => %s [null] %n", method.getName (), result);
    // }
    // else
    // {
    // System.out.printf ("* Invoke method %s('%s') => %s %s%n", method.getName (), arg, result,
    // result.getClass ());
    // }
    // }
    // }
    // }
    // System.out.printf ("%n");
    // }

    /**
     * Get the name from created names to quoted objects. This only works because compiled code is
     * being loaded into the same environment where it is compiled. To save compiled code to a file
     * would required building the structure in the init method when the class is loaded.
     */
    public Map<String, Object> getQuotedReferences ()
    {
	return quotedReferences;
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
