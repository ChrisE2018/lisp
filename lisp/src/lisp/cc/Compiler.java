
package lisp.cc;

import java.io.IOException;

public interface Compiler
{
    public Class<?> compile () throws IOException;
}
