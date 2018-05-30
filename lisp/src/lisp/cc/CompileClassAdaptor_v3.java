
package lisp.cc;

import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.symbol.*;
import lisp.util.LogString;

public class CompileClassAdaptor_v3 extends ClassVisitor implements Opcodes, CompilerGenerator
{
    private static final Logger LOGGER = Logger.getLogger (CompileLoader_v2.class.getName ());

    private static Convert convert = new Convert ();

    private final Type shellClassType;
    private final Class<?> returnClass;
    private final Type returnType;
    private final String methodName;
    private final List<Symbol> methodArgs = new ArrayList<Symbol> ();
    private final List<Class<?>> methodArgClasses = new ArrayList<Class<?>> ();
    private final List<Type> methodArgTypes = new ArrayList<Type> ();
    private final List<Object> methodBody;
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();
    private final Map<Object, Symbol> quotedReferences = new LinkedHashMap<Object, Symbol> ();

    /**
     * Inverse map of quoted references. This is created in the CompileLoader and saved here. When
     * it is modified, the class can get at it by getClassLoader().getQuotedReferences ();
     */
    private final Map<String, Object> quotedReferencesMap;

    private Map<Symbol, LocalBinding> localVariableMap = new LinkedHashMap<Symbol, LocalBinding> ();

    public CompileClassAdaptor_v3 (final ClassVisitor cv, final Type shellClassType, final Class<?> returnClass,
            final String methodName, final LispList methodArgs, final LispList methodBody,
            final Map<String, Object> quotedReferencesMap)
    {
	super (Opcodes.ASM5, cv);
	this.shellClassType = shellClassType;
	this.returnClass = returnClass;
	returnType = Type.getType (returnClass);
	this.methodName = methodName;
	for (final Object arg : methodArgs)
	{
	    final Symbol variable = CompileSupport.getNameVariable (arg);
	    final Class<?> varClass = CompileSupport.getNameType (arg);
	    final Type varType = Type.getType (varClass);
	    this.methodArgs.add (variable);
	    methodArgClasses.add (varClass);
	    methodArgTypes.add (varType);
	}
	this.methodBody = methodBody;
	this.quotedReferencesMap = quotedReferencesMap;
    }

    /** get the ASM type of the class being generated. */
    @Override
    public Type getClassType ()
    {
	return shellClassType;
    }

    /** Does the symbol name a method argument. */
    @Override
    public boolean isMethodArg (final Symbol symbol)
    {
	return methodArgs.contains (symbol);
    }

    /** Get the declared class of a method argument. */
    @Override
    public Class<?> getMethodArgClass (final Symbol symbol)
    {
	final int argIndex = methodArgs.indexOf (symbol);
	return methodArgClasses.get (argIndex);
    }

    /** Get the position of a method argument. */
    @Override
    public int getMethodArgIndex (final Symbol symbol)
    {
	return methodArgs.indexOf (symbol);
    }

    /** Get binding information about a local variable. */
    @Override
    public LocalBinding getLocalVariableBinding (final Symbol symbol)
    {
	return localVariableMap.get (symbol);
    }

    // private final Deque<Map<Symbol, LocalBinding>> localBindingStack = new LinkedList<Map<Symbol,
    // LocalBinding>> ();

    /** Get the current local binding context. */
    public Map<Symbol, LocalBinding> getLocalBindingContext ()
    {
	return localVariableMap;
    }

    /** Set the current local binding context. */
    public void setLocalBindingContext (final Map<Symbol, LocalBinding> variableMap)
    {
	localVariableMap = variableMap;
    }

    /** Keep track of a symbol that needs to be available as a class field. */
    @Override
    public void addSymbolReference (final Symbol symbol)
    {
	if (!symbolReferences.contains (symbol))
	{
	    symbolReferences.add (symbol);
	}
    }

    /** Keep track of a symbol that has a global reference. */
    @Override
    public void addGlobalReference (final Symbol symbol)
    {
	if (!globalReferences.contains (symbol))
	{
	    globalReferences.add (symbol);
	    LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
	}
    }

    /**
     * Finish the class visit. This method produces the fields for all references and an init method
     * to set them up. (define foo (x) (+ x a))
     */
    @Override
    public void visitEnd ()
    {
	compileDefinition ();

	// Create field definitions for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String name = createJavaSymbolName (symbol);
	    final String typeDescriptor = Type.getType (symbol.getClass ()).getDescriptor ();
	    createField (ACC_PRIVATE, name, typeDescriptor);
	    // LOGGER.finer (new LogString ("Field: private Symbol %s; [%s]", name, symbol));
	}
	for (final Entry<Object, Symbol> entry : quotedReferences.entrySet ())
	{
	    final Object quoted = entry.getKey ();
	    final Symbol reference = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    // LOGGER.finer (new LogString ("Field: private Quoted %s; [%s]", reference, quoted));
	    createField (ACC_PRIVATE, reference.getName (), typeDescriptor);
	}
	// Create init method as the very last step, so all requirements from other compilation
	// steps are known
	createInitI ();
	cv.visitEnd ();
    }

    private void compileDefinition ()
    {
	final String signature = getMethodSignature ();
	// With LocalVariablesSorter we can allocate new local variables.
	// For example:
	// int time = newLocal(Type.LONG_TYPE);
	// creates a variable entry for a long that can be used like this:
	// mv.visitVarInsn(LSTORE, time);
	// final LocalVariablesSorter mv =
	// new LocalVariablesSorter (ACC_PUBLIC, signature, cv.visitMethod (ACC_PUBLIC, methodName,
	// signature, null, null));
	final GeneratorAdapter mv = new GeneratorAdapter (cv.visitMethod (ACC_PUBLIC, methodName, signature, null, null),
	        ACC_PUBLIC, methodName, signature);
	// Compile method body
	final int bodyLimit = methodBody.size () - 1;
	for (int i = 0; i < bodyLimit; i++)
	{
	    final Object e = methodBody.get (i);
	    compileExpression (mv, e, null, false, false);
	}
	final Object e = methodBody.get (bodyLimit);
	compileExpression (mv, e, returnClass, false, false);

	mv.visitInsn (returnType.getOpcode (IRETURN));
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

    private String getMethodSignature ()
    {
	final String returnTypeDescriptor = returnType.getDescriptor ();
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("(");
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    // buffer.append (CompileSupport.getNameTypeDescriptor (methodArgTypes.get (i)));
	    buffer.append (Type.getType (methodArgClasses.get (i)).getDescriptor ());
	}
	buffer.append (")");
	buffer.append (returnTypeDescriptor);
	return buffer.toString ();
    }

    /**
     * The int constructor is added to the CompiledShell class. Removing the no-arg constructor
     * proved to be difficult, but this is easy. This constructor should create fields for all
     * Symbol and structure references needed by the functions. Support classes might have to be
     * used as a repository to hold references to complex structures so they don't get copied.
     */
    private void createInitI ()
    {
	final String classInternalName = shellClassType.getInternalName ();
	final Type classLoaderType = Type.getType (CompileLoader.class);
	final String classLoaderInternalName = classLoaderType.getInternalName ();
	final Type symbolType = Type.getType (Symbol.class);
	final String symbolTypeDescriptor = symbolType.getDescriptor ();
	final Type stringType = Type.getType (String.class);
	final Type objectType = Type.getType (Object.class);
	final Type classType = Type.getType (Class.class);
	System.out.printf ("classLoaderInternalName %s %n", classLoaderInternalName);
	LOGGER.finer (new LogString ("Creating <init>(int) method for %s", classInternalName));
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, "<init>", "(I)V", null, null);
	mv.visitCode ();

	// Call super constructor.
	mv.visitVarInsn (ALOAD, 0);
	mv.visitMethodInsn (INVOKESPECIAL, objectType.getInternalName (), "<init>", "()V", false);

	// Store the methodName in a field
	mv.visitVarInsn (ALOAD, 0);
	mv.visitLdcInsn (methodName);
	mv.visitFieldInsn (PUTFIELD, classInternalName, "methodName", stringType.getDescriptor ());

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaName = createJavaSymbolName (symbol);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitLdcInsn (symbol.getPackage ().getName ());
	    mv.visitLdcInsn (symbol.getName ());
	    mv.visitMethodInsn (INVOKESPECIAL, classInternalName, "getSymbol",
	            Type.getMethodDescriptor (symbolType, stringType, stringType), false);
	    mv.visitFieldInsn (PUTFIELD, classInternalName, javaName, symbolTypeDescriptor);

	    LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaName, symbol));
	}
	for (final Entry<Object, Symbol> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    final Object quoted = entry.getKey ();
	    final Symbol reference = entry.getValue ();
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitInsn (DUP);
	    mv.visitMethodInsn (INVOKEVIRTUAL, objectType.getInternalName (), "getClass", Type.getMethodDescriptor (classType),
	            false);
	    mv.visitMethodInsn (INVOKEVIRTUAL, classType.getInternalName (), "getClassLoader", "()Ljava/lang/ClassLoader;",
	            false);
	    mv.visitTypeInsn (CHECKCAST, classLoaderInternalName);
	    mv.visitMethodInsn (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;", false);

	    mv.visitLdcInsn (reference.getName ());
	    mv.visitMethodInsn (INVOKEINTERFACE, "java/util/Map", "get", Type.getMethodDescriptor (objectType, objectType), true);
	    final Type quotedType = Type.getType (quoted.getClass ());
	    final String typeDescriptor = quotedType.getDescriptor ();
	    mv.visitTypeInsn (CHECKCAST, quotedType.getInternalName ());
	    mv.visitFieldInsn (PUTFIELD, classInternalName, reference.getName (), typeDescriptor);
	}
	mv.visitInsn (RETURN);
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

    /**
     * Compile a single expression and leave the value on top of the stack. This is the primary
     * compilation operation and is used recursively to compile all expressions.
     *
     * @param mv Bytecode generator.
     * @param e Expression to compile.
     * @param valueType Class of value to leave on the stack.
     * @param allowNarrowing When true, narrowing conversions will be generated if required.
     *            Otherwise narrowing throws and error.
     * @param liberalTruth When set, any non-boolean result is accepted as true. Otherwise, boolean
     *            testing requires strictly boolean values.
     */
    public void compileExpression (final GeneratorAdapter mv, final Object e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (e == null)
	{
	    throw new Error ("Null is an illegal expression");
	}
	else if (e instanceof LispList)
	{
	    compileFunctionCall (mv, (LispList)e, valueType, allowNarrowing, liberalTruth);
	}
	else if (valueType != null)
	{
	    if (e instanceof Symbol)
	    {
		final Symbol symbol = (Symbol)e;
		compileSymbolReference (mv, symbol, valueType, allowNarrowing, liberalTruth);
	    }
	    else
	    {
		compileConstantExpression (mv, e, valueType, allowNarrowing, liberalTruth);
	    }
	}
    }

    private void compileSymbolReference (final GeneratorAdapter mv, final Symbol symbol, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (methodArgs.contains (symbol))
	{
	    // Parameter reference
	    if (valueType != null)
	    {
		final int argIndex = methodArgs.indexOf (symbol);
		mv.loadArg (argIndex);
		final Class<?> fromClass = methodArgClasses.get (argIndex);
		final Class<?> toClass = valueType;
		convert.convert (mv, fromClass, toClass, allowNarrowing, liberalTruth);
	    }
	}
	else if (localVariableMap.containsKey (symbol))
	{
	    if (valueType != null)
	    {
		// [TODO] If we can determine the type, use that information.
		final LocalBinding lb = localVariableMap.get (symbol);
		final int localRef = lb.getLocalRef ();
		mv.loadLocal (localRef);
		final Class<?> fromClass = lb.getClass ();
		final Class<?> toClass = valueType;
		convert.convert (mv, fromClass, toClass, allowNarrowing, liberalTruth);
	    }
	}
	else if (symbol.is ("true") && valueType.equals (boolean.class))
	{
	    // Special case for symbol "true"
	    mv.visitLdcInsn (true);
	}
	else if (symbol.is ("false") && valueType.equals (boolean.class))
	{
	    // Special case for symbol "false"
	    mv.visitLdcInsn (false);
	}
	else
	{
	    if (valueType != null)
	    {
		if (!globalReferences.contains (symbol))
		{
		    globalReferences.add (symbol);
		    LOGGER.finer (new LogString ("Compiled global reference to %s", symbol));
		}
		if (!symbolReferences.contains (symbol))
		{
		    symbolReferences.add (symbol);
		    LOGGER.finer (new LogString ("Symbol reference to %s", symbol));
		}
		mv.visitVarInsn (ALOAD, 0);
		final String classInternalName = shellClassType.getInternalName ();
		// [TODO] If the symbol valueCell is constant, use the current value.
		// [TODO] If the valueCell is a TypedValueCell, use the type information.
		mv.visitFieldInsn (GETFIELD, classInternalName, createJavaSymbolName (symbol), "Llisp/Symbol;");
		mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false);
		// [TODO] Implement allowNarrowing and liberalTruth here.
		coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
	    }
	}
    }

    private void compileConstantExpression (final MethodVisitor mv, final Object e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (valueType.equals (boolean.class))
	{
	    if (e instanceof Boolean && !((Boolean)e).booleanValue ())
	    {
		mv.visitInsn (ICONST_0);
		return;
	    }
	    if (liberalTruth)
	    {
		mv.visitInsn (ICONST_1);
		return;
	    }
	    throw new IllegalArgumentException ("Constant " + e + " is not a boolean");
	}
	// Compile constant expressions
	// [TODO] All of these box the constant in a class wrapper. If we can use the primitive
	// type instead, that is more efficient.
	else if (e instanceof Boolean)
	{
	    mv.visitLdcInsn (e);
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(B)Ljava/lang/Boolean;", false);
	}
	else if (e instanceof Byte)
	{
	    final Byte b = (Byte)e;
	    if (valueType.equals (byte.class))
	    {
		mv.visitLdcInsn (b.byteValue ());
	    }
	    else if (valueType.equals (short.class))
	    {
		mv.visitLdcInsn (b.shortValue ());
	    }
	    else if (valueType.equals (int.class))
	    {
		mv.visitLdcInsn (b.intValue ());
	    }
	    else if (valueType.equals (long.class))
	    {
		mv.visitLdcInsn (b.longValue ());
	    }
	    else if (valueType.equals (float.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (b.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (b.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (b);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false);
	    }
	}
	else if (e instanceof Short)
	{
	    final Short s = (Short)e;
	    if (valueType.equals (short.class))
	    {
		mv.visitLdcInsn (s.shortValue ());
	    }
	    else if (valueType.equals (int.class))
	    {
		mv.visitLdcInsn (s.intValue ());
	    }
	    else if (valueType.equals (long.class))
	    {
		mv.visitLdcInsn (s.longValue ());
	    }
	    else if (valueType.equals (float.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (s.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		// Does this make sense?
		mv.visitLdcInsn (s.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (s);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false);
	    }
	}
	else if (e instanceof Integer)
	{
	    final Integer i = (Integer)e;
	    if (valueType.equals (int.class))
	    {
		mv.visitLdcInsn (i.intValue ());
	    }
	    else if (valueType.equals (byte.class))
	    {
		mv.visitLdcInsn (i.byteValue ());
	    }
	    else if (valueType.equals (short.class))
	    {
		mv.visitLdcInsn (i.shortValue ());
	    }
	    else if (valueType.equals (long.class))
	    {
		mv.visitLdcInsn (i.longValue ());
	    }
	    else if (valueType.equals (float.class))
	    {
		mv.visitLdcInsn (i.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		mv.visitLdcInsn (i.doubleValue ());
	    }
	    else
	    {
		// This can produce bad results if valueType is not Object.
		mv.visitLdcInsn (i);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	    }
	}
	else if (e instanceof Long)
	{
	    final Long l = (Long)e;
	    if (valueType.equals (int.class))
	    {
		mv.visitLdcInsn (l.intValue ());
	    }
	    else if (valueType.equals (byte.class))
	    {
		mv.visitLdcInsn (l.byteValue ());
	    }
	    else if (valueType.equals (short.class))
	    {
		mv.visitLdcInsn (l.shortValue ());
	    }
	    else if (valueType.equals (long.class))
	    {
		mv.visitLdcInsn (l.longValue ());
	    }
	    else if (valueType.equals (float.class))
	    {
		mv.visitLdcInsn (l.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		mv.visitLdcInsn (l.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (l);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false);
	    }
	}
	// [TODO] Continue the same code pattern...
	else if (e instanceof Float)
	{
	    final Float f = (Float)e;
	    if (valueType.equals (float.class))
	    {
		mv.visitLdcInsn (f.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		mv.visitLdcInsn (f.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false);
	    }
	}
	else if (e instanceof Double)
	{
	    final Double d = (Double)e;
	    if (valueType.equals (float.class))
	    {
		mv.visitLdcInsn (d.floatValue ());
	    }
	    else if (valueType.equals (double.class))
	    {
		mv.visitLdcInsn (d.doubleValue ());
	    }
	    else
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
	    }
	}
	else if (e instanceof String)
	{
	    mv.visitLdcInsn (e);
	}
	else
	{
	    LOGGER.info (new LogString ("Ignoring '%s' %s", e, e.getClass ()));
	    mv.visitInsn (ACONST_NULL);
	}
    }

    private void compileFunctionCall (final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// Need to be able to compile a call to an undefined function (i.e. recursive call)
	LOGGER.finer (new LogString ("Compile nested form %s", e));
	if (e.size () == 0)
	{
	    throw new Error ("No function in function call");
	}
	final Object head = e.get (0);
	if (!(head instanceof Symbol))
	{
	    throw new IllegalArgumentException ("Function is not a symbol " + head);
	}
	// [TODO] Consider alternatives of pushing this code into the compiled function
	// vs making these choices at compile time. The compile time choice will produce
	// faster code, but it won't be able to change behavior of the called function
	// is changed. This whole block of functionality could be a private method in
	// the CompiledShell class. This would evaluate the arguments into a List/Array
	// and pass them in. The compiler should keep track of all the function
	// definitions it uses, and also keep the source expression. If any function
	// definition changes, the code should be recompiled.
	final Symbol f = (Symbol)head;
	final FunctionCell function = f.getFunction ();
	if (function != null && function instanceof SpecialFunctionCell)
	{
	    // Unless this is a known special function, we are stuck and can't
	    // proceed
	    compileSpecialFunctionCall (mv, f, e, valueType, allowNarrowing, liberalTruth);
	}
	else if (function != null && function instanceof MacroFunctionCell)
	{
	    // Expand and replace
	    final MacroFunctionCell macro = (MacroFunctionCell)function;
	    Object replacement;
	    try
	    {
		replacement = macro.expand (e);
	    }
	    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e1)
	    {
		throw new Error ("Error expanding macro " + f, e1);
	    }
	    compileExpression (mv, replacement, valueType, allowNarrowing, liberalTruth);
	}
	else
	{
	    // [TODO] Some 'normal' functions need special coding, i.e, arithmetic and comparisons.
	    compileStandardFunctionCall (mv, f, e, valueType, allowNarrowing, liberalTruth);
	}
    }

    // (define foo () (not true))
    private void compileStandardFunctionCall (final GeneratorAdapter mv, final Symbol symbol, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// Save the symbol in a class field.
	// [TODO] If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.

	if (!symbolReferences.contains (symbol))
	{
	    symbolReferences.add (symbol);
	}
	LOGGER.finer (new LogString ("Function symbol reference to %s", symbol));

	mv.visitVarInsn (ALOAD, 0);
	final String classInternalName = shellClassType.getInternalName ();
	mv.visitFieldInsn (GETFIELD, classInternalName, createJavaSymbolName (symbol), "Llisp/Symbol;");

	// The call to getDefaultHandlerFunction will return a DefaultHandler that tries to invoke
	// the java method on arg 1 if the function has not been given any other definition.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getDefaultHandlerFunction", "()Llisp/symbol/FunctionCell;", false);
	// Compile the arguments
	final int argCount = e.size () - 1;
	ldcGeneral (mv, argCount);
	mv.visitTypeInsn (ANEWARRAY, "java/lang/Object");
	for (int i = 0; i < argCount; i++)
	{
	    // [TODO] If we know argument types of the function we are about to call we can try to
	    // compile the expression more efficiently.
	    mv.visitInsn (DUP);
	    ldcGeneral (mv, i);
	    compileExpression (mv, e.get (i + 1), Object.class /* TODO */, false, false);
	    mv.visitInsn (AASTORE);
	}
	// Call invoke on the method.
	// Assume the function will still be defined when we execute this code.
	// [TODO] Could define an applyVoid method to return no value.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;", false);
	coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    }

    private void compileSpecialFunctionCall (final GeneratorAdapter mv, final Symbol symbol, final LispList expression,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (getAllSpecialFunctionSymbols)
	// Done: (quote progn when if unless and or setq repeat while until let let* cond)
	// Done: Calls to new & static
	// Todo: (try)
	// Skip: (def verify verifyError define)
	// Future: (loop block return block-named)
	// Think: Multiple value calls?
	// [done] Calls to Java methods
	// Defmacro
	// &optional, &key, &rest
	// Hookup definition of special function calls to DefineLisp annotation.

	final FunctionCell function = symbol.getFunction ();
	final ObjectMethod compiler = function.getCompiler ();
	if (compiler != null)
	{
	    final Object object = compiler.getObject ();
	    final Method method = compiler.getMethod ();
	    try
	    {
		method.invoke (object, this, mv, expression, valueType, allowNarrowing, liberalTruth);
	    }
	    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
	    {
		throw new Error ("Internal compiler error", e);
	    }
	    return;
	}
	throw new IllegalArgumentException ("Unrecognized special form " + symbol);
    }

    @Override
    public void addQuotedConstant (final Symbol reference, final Object quoted)
    {
	if (!quotedReferences.containsKey (quoted))
	{
	    // Save the reference and build it in the init method.
	    quotedReferences.put (quoted, reference);
	    quotedReferencesMap.put (reference.getName (), quoted);
	}
    }

    private void createField (final int fieldAccess, final String fieldName, final String fieldDescriptor)
    {
	// Field initial value only applies to static fields
	final FieldVisitor fv = cv.visitField (fieldAccess, fieldName, fieldDescriptor, null, null);
	if (fv != null)
	{
	    fv.visitEnd ();
	}
    }

    /** Load a constant using the quick version when value is small enough. */
    private void ldcGeneral (final MethodVisitor mv, final int i)
    {
	if (i <= 5)
	{
	    mv.visitInsn (ICONST_0 + i);
	}
	else
	{
	    mv.visitLdcInsn (i);
	}
    }

    /** Convert class to type and coerce the value. */
    @Override
    public void coerceRequired (final GeneratorAdapter mv, final Class<?> valueClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (valueClass == null)
	{
	    coerceRequired (mv, Type.VOID_TYPE, allowNarrowing, liberalTruth);
	}
	else
	{
	    coerceRequired (mv, Type.getType (valueClass), allowNarrowing, liberalTruth);
	}
    }

    /**
     * Convert an instance of a boxed wrapper class into the corresponding primitive type. Note:
     * short and byte are converted to int. See ByteCodeUtils if this is a problem. The stack must
     * contain an instance of the wrapperType. This will work for Boolean since it won't convert
     * other instances to true.
     */
    private void coerceRequired (final GeneratorAdapter mv, final Type valueType, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	final int sort = valueType.getSort ();
	if (sort == Type.VOID)
	{
	    mv.visitInsn (POP);
	}
	else if (valueType.equals (Type.BOOLEAN_TYPE))
	{
	    // Treat anything except Boolean as true.
	    // unbox booleans
	    // [TODO] Implement liberalTruth here
	    convert.coerceBoolean (mv);
	}
	else if (sort > Type.VOID && sort < Type.ARRAY)
	{
	    // Call unbox with the type that we want to end up with
	    mv.unbox (valueType);
	}
	else
	{
	    // Leave object types alone
	}
    }

    /**
     * Push a default value onto the stack.
     *
     * @param mv GeneratorAdapter to produce code.
     * @param valueClass The value type to return.
     * @param booleanDefault If the value will be a primitive boolean, use this as the default
     *            value.
     */
    public void pushDefaultValue (final GeneratorAdapter mv, final Class<?> valueClass, final boolean booleanDefault)
    {
	if (valueClass != null)
	{
	    if (boolean.class.equals (valueClass))
	    {
		mv.visitLdcInsn (booleanDefault);
	    }
	    else if (byte.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Byte.valueOf ((byte)0));
	    }
	    else if (char.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Character.valueOf ((char)0));
	    }
	    else if (short.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Short.valueOf ((short)0));
	    }
	    else if (int.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Integer.valueOf (0));
	    }
	    else if (long.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Long.valueOf (0));
	    }
	    else if (float.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Float.valueOf (0.0f));
	    }
	    else if (double.class.equals (valueClass))
	    {
		mv.visitLdcInsn (Double.valueOf (0.0));
	    }
	    else
	    {
		mv.visitInsn (ACONST_NULL);
	    }
	}

    }

    /**
     * Turn a symbol name into something acceptable to Java. Lisp symbols can include characters
     * like '+' that are not allowed in Java.
     */
    @Override
    public String createJavaSymbolName (final Symbol symbol)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("SYM_");
	final String name = symbol.getName ();
	for (int i = 0; i < name.length (); i++)
	{
	    final char c = name.charAt (i);
	    if (Character.isJavaIdentifierPart (c))
	    {
		buffer.append (c);
	    }
	    else
	    {
		final int codePoint = name.codePointAt (i);
		final String cn = Character.getName (codePoint);
		if (cn != null)
		{
		    buffer.append (c);
		}
		else
		{
		    buffer.append (String.format ("%03d", codePoint));
		}
	    }
	}
	return buffer.toString ();
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
	buffer.append (methodName);
	buffer.append (">");
	return buffer.toString ();
    }
}
