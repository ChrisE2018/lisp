
package lisp.cc;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.LispList;
import lisp.util.LogString;

public class CompileLoader_v2 extends ClassLoader implements Compiler
{
    private static final Logger LOGGER = Logger.getLogger (CompileLoader_v2.class.getName ());

    /**
     * Predefined shell class structure with support methods. To create a compiled function we load
     * this shell class and inject our new method. Each compile requires a new instance of this
     * ClassLoaded so we get a distinct class as a result. <br/>
     * Maybe use a gensym in the defineClass call to allow ClassLoader re-use?
     */
    private static final String SHELL_CLASS_DESCRIPTOR = "Llisp/cc/CompiledShell;";
    private final Type classType = Type.getType (SHELL_CLASS_DESCRIPTOR);

    /**
     * Trick to compile references to quoted data. This map is obtained by the method compiler and
     * changed by side effect using unique gensym keys. The bytecode uses the gensym Symbol name to
     * locate the quoted data. This works only because we are loading compiled bytecode into the
     * same instance of the JVM that we used to compile the code. If we need to save compiled code
     * to a file and load it later, the quoted structure would have to be reconstructed at load
     * time.
     */
    private final Map<String, Object> quotedReferences = new HashMap<String, Object> ();
    private final ClassReader cr;
    private ClassVisitor cv;

    private final ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);

    public CompileLoader_v2 (final Class<?> returnType, final String methodName, final LispList methodArgs,
            final LispList methodBody) throws IOException
    {

	final String className = classType.getClassName ();
	final String classInternalName = classType.getInternalName ();

	LOGGER.info (new LogString ("Creating compiled class: %s for function %s %s %s", className, returnType, methodName,
	        methodArgs));
	LOGGER.info (new LogString ("Adding method %s %s to class: %s", methodName, methodArgs, className));
	final String resource = classInternalName + ".class";
	final InputStream is = getResourceAsStream (resource);
	cr = new ClassReader (is);
	cv = new CompileClassAdaptor_v2 (cw, classType, returnType, methodName, methodArgs, methodBody, quotedReferences);
    }

    public ClassVisitor getClassVisitor ()
    {
	return cv;
    }

    public void setClassVisitor (final ClassVisitor cv)
    {
	this.cv = cv;
    }

    /**
     * Load the shell class resource and run the ClassVisitor on it to add our new method.
     */
    public Class<?> compile () throws IOException
    {
	cr.accept (cv, 0);

	final byte[] b = cw.toByteArray ();
	final String className = classType.getClassName ();
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
