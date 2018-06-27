
package lisp.cc;

import java.util.Map;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.lang.Symbol;

public interface QuotedData
{
    /** Keep track of a symbol that needs to be available as a class field. */
    public void addSymbolReference (final Symbol symbol);

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param quoted The quoted data to be stored.
     * @return The symbol that will name the data field. This is a generated unique symbol.
     */
    public Symbol addQuotedConstant (final Object quoted);

    /** Get the saved quoted data. */
    public Map<Symbol, Object> getQuotedData ();

    public void addRequiredFields (ClassNode cn);

    public void addHiddenConstructorSteps (Type classType, final MethodVisitor mv);

    /** Load quoted data onto the stack. */
    public void loadData (Symbol s, final String classInternalName, InsnList il);
}
