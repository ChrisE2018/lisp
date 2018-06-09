
package lisp.cc;

import java.io.IOException;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.*;

public interface Compiler
{
    // Where does this belong?
    public static int ASM_VERSION = Opcodes.ASM5;

    public Class<?> compile () throws IOException;

    /** Provide access to ASM internals if possible. */
    public ClassNode getClassNode ();

    /** Provide access to ASM internals if possible. */
    public MethodNode getMethodNode ();
}
