
package lisp.cc4;

import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.cc.Compiler;
import lisp.lang.Symbol;

public class CompileLoaderV4 extends ClassLoader implements Compiler
{
    // private static final Logger LOGGER = Logger.getLogger (CompileLoaderV4.class.getName ());

    /**
     * Trick to compile references to quoted data. This map is obtained by the method compiler and
     * changed by side effect using unique gensym keys. The bytecode uses the gensym Symbol name to
     * locate the quoted data. This works only because we are loading compiled bytecode into the
     * same instance of the JVM that we used to compile the code. If we need to save compiled code
     * to a file and load it later, the quoted structure would have to be reconstructed at load
     * time.
     */
    private final Map<Symbol, Object> quotedReferences = new LinkedHashMap<Symbol, Object> ();
    private ClassReader cr = null;

    // ClassWriter flags Must be zero or more of {@link #COMPUTE_MAXS} and {@link #COMPUTE_FRAMES}.
    private ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);
    private ClassVisitor cv = cw;

    /** The ASM type of the class enclosing the function currently being compiled. */
    private Type classType = null;

    /** Save output here. */
    private ClassNode classNode = null;
    private MethodNode methodNode = null;

    public CompileLoaderV4 ()
    {
    }

    /** The ASM type of the class enclosing the function currently being compiled. */
    public Type getClassType ()
    {
	return classType;
    }

    public void setClassType (final Type classType)
    {
	this.classType = classType;
    }

    public ClassReader getClassReader ()
    {
	return cr;
    }

    public void setClassReader (final ClassReader cr)
    {
	this.cr = cr;
    }

    public ClassWriter getClassWriter ()
    {
	return cw;
    }

    public void setClassWriter (final ClassWriter cw)
    {
	this.cw = cw;
    }

    public ClassVisitor getClassVisitor ()
    {
	return cv;
    }

    public void setClassVisitor (final ClassVisitor cv)
    {
	this.cv = cv;
    }

    /** Provide access to ASM internals if possible. */
    public ClassNode getClassNode ()
    {
	return classNode;
    }

    public void setClassNode (final ClassNode classNode)
    {
	this.classNode = classNode;
    }

    /** Provide access to ASM internals if possible. */
    public MethodNode getMethodNode ()
    {
	return methodNode;
    }

    public void setMethodNode (final MethodNode methodNode)
    {
	this.methodNode = methodNode;
    }

    /**
     * Get the name from created names to quoted objects. This only works because compiled code is
     * being loaded into the same environment where it is compiled. To save compiled code to a file
     * would required building the structure in the init method when the class is loaded.
     */
    public Map<String, Object> getQuotedReferences ()
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    result.put (entry.getKey ().getName (), entry.getValue ());
	}
	return result;
    }

    public Map<Symbol, Object> getQuotedData ()
    {
	return quotedReferences;
    }

    /**
     * Load the shell class resource and run the ClassVisitor on it to add our new method.
     */
    public Class<?> compile () throws IOException
    {
	if (cr == null)
	{
	    final String name = classType.getInternalName ();
	    // cv.visit (version, access, name, signature, superName, interfaces);
	    cv.visit (Opcodes.V1_5, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", new String[] {});
	    cv.visitEnd ();
	}
	else
	{
	    cr.accept (cv, 0);
	}

	final byte[] b = cw.toByteArray ();
	final String className = classType.getClassName ();
	final Class<?> c = defineClass (className, b, 0, b.length);
	return c;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (" ");
	buffer.append (classType);
	buffer.append (">");
	return buffer.toString ();
    }
}
