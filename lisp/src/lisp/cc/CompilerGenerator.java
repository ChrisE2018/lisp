
package lisp.cc;

import java.util.Map;

import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.Symbol;

public interface CompilerGenerator
{
    /** get the ASM type of the class being generated. */
    public Type getClassType ();

    /**
     * Turn a symbol name into something acceptable to Java. Lisp symbols can include characters
     * like '+' that are not allowed in Java.
     */
    public String createJavaSymbolName (final Symbol symbol);

    /** Keep track of a symbol that needs to be available as a class field. */
    public void addSymbolReference (Symbol symbol);

    /** Keep track of a symbol that has a global reference. */
    public void addGlobalReference (Symbol symbol);

    /** Does the symbol name a method argument. */
    public boolean isMethodArg (Symbol symbol);

    /** Get the declared class of a method argument. */
    public Class<?> getMethodArgClass (Symbol symbol);

    /** Get the position of a method argument. */
    public int getMethodArgIndex (Symbol symbol);

    /** Get binding information about a local variable. */
    public LocalBinding getLocalVariableBinding (Symbol symbol);

    /** Get the current local binding context. */
    public Map<Symbol, LocalBinding> getLocalBindingContext ();

    /** Set the current local binding context. */
    public void setLocalBindingContext (Map<Symbol, LocalBinding> variableMap);

    /**
     * Compile a single expression and leave the value on top of the stack. This is the primary
     * compilation operation and is used recursively to compile all expressions.
     *
     * @param mv Bytecode generator.
     * @param e Expression to compile.
     * @param valueClass Class of value to leave on the stack.
     * @param allowNarrowing When true, narrowing conversions will be generated if required.
     *            Otherwise narrowing throws and error.
     * @param liberalTruth When set, any non-boolean result is accepted as true. Otherwise, boolean
     *            testing requires strictly boolean values.
     */
    public void compileExpression (GeneratorAdapter mv, Object expression, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth);

    /**
     * Push a default value onto the stack.
     *
     * @param mv GeneratorAdapter to produce code.
     * @param valueClass The value type to return.
     * @param booleanDefault If the value will be a primitive boolean, use this as the default
     *            value.
     */
    public void pushDefaultValue (GeneratorAdapter mv, Class<?> valueClass, final boolean booleanDefault);

    /** Generate code to convert the top stack element to a required valueClass. */
    @Deprecated
    public void coerceRequired (final GeneratorAdapter mv, final Class<?> valueClass);

    /** Define a field to contain quoted data. */
    public void addQuotedConstant (final Symbol reference, final Object quoted);

}
