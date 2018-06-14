/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.cc4;

import org.objectweb.asm.Type;

import lisp.Symbol;

/**
 * @author cre
 */
public interface TreeCompilerInterface
{
    // /**
    // * The return value class of the function currently being compiled.
    // *
    // * @return The class of the function return value.
    // */
    // public Class<?> getMethodReturnClass ();

    /** The ASM type of the class enclosing the function currently being compiled. */
    public Type getClassType ();

    /** Keep track of a symbol that needs to be available as a class field. */
    public void addSymbolReference (final Symbol symbol);

    /**
     * Keep track of a symbol that has a global reference. This is only used to produce a log
     * message. globalReferences does nothing else.
     */
    public void addGlobalReference (final Symbol symbol);

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param reference The symbol that will name the data field.
     * @param quoted The quoted data to be stored.
     */
    public void addQuotedConstant (final Symbol reference, final Object quoted);

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param quoted The quoted data to be stored.
     * @return The symbol that will name the data field. This is a generated unique symbol.
     */
    public Symbol addQuotedConstant (final Object quoted);
}
