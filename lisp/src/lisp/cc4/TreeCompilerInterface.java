/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.cc4;

import org.objectweb.asm.Type;

/**
 * @author cre
 */
public interface TreeCompilerInterface
{
    /** The ASM type of the class enclosing the function currently being compiled. */
    public Type getClassType ();

    // /** Keep track of a symbol that needs to be available as a class field. */
    // public void addSymbolReference (final Symbol symbol);
}
