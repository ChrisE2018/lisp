
package lisp.cc;

import java.io.IOException;

import org.objectweb.asm.Opcodes;

public interface Compiler
{
    // Where does this belong?
    public static int ASM_VERSION = Opcodes.ASM5;

    public Class<?> compile () throws IOException;
}
