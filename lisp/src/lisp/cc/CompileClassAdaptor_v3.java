
package lisp.cc;

import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.*;
import lisp.Package;
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
	    compileExpression (mv, e, null);
	}
	final Object e = methodBody.get (bodyLimit);
	compileExpression (mv, e, returnClass);

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
     */
    private void compileExpression (final GeneratorAdapter mv, final Object e, final Class<?> valueType)
    {
	compileExpression (mv, e, valueType, false, false);
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
	// [TOOD] valueType is ignored for now except when it is null or boolean.class
	// [TODO] valueType should be supported for all primitive types
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
	    compileExpression (mv, e.get (i + 1), Object.class /* TODO */);
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

	// if (symbol.is ("quote"))
	// {
	// compileQuote (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("progn"))
	// {
	// compileProgn (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("if"))
	// {
	// if (expression.size () <= 3)
	// { // No else clause, compile as when
	// compileWhen (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else
	// { // If then else
	// compileIf (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// }
	// else if (symbol.is ("and"))
	// {
	// if (valueType == null)
	// {
	// compileVoidAnd (mv, expression);
	// }
	// else if (valueType.equals (boolean.class))
	// {
	// compileBooleanAnd (mv, expression);
	// }
	// else
	// {
	// compileAnd (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// }
	// else if (symbol.is ("or"))
	// {
	// if (valueType == null)
	// {
	// compileVoidOr (mv, expression);
	// }
	// else if (valueType.equals (boolean.class))
	// {
	// compileBooleanOr (mv, expression);
	// }
	// else
	// {
	// compileOr (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// }
	// else if (symbol.is ("when"))
	// {
	// compileWhen (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("unless"))
	// {
	// compileUnless (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("setq"))
	// {
	// compileSetq (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("repeat"))
	// {
	// compileRepeat (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("dotimes"))
	// {
	// compileDotimes (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("while"))
	// {
	// compileWhile (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	// else if (symbol.is ("until"))
	// {
	// compileUntil (mv, expression, valueType, allowNarrowing, liberalTruth);
	// }
	else if (symbol.is ("let"))
	{
	    compileLet (mv, expression, valueType, allowNarrowing, liberalTruth);
	}
	else if (symbol.is ("let*"))
	{
	    // [CONSIDER] Change reader so that a:b reads as (the a b) and define:
	    // (the <type> <reference>)
	    // to be a type declaration. Then:
	    // (define int:foo (int : a int : b) (+ a b))
	    // becomes integer addition. (Problem, colon is already used as a package separator.
	    // Maybe change that to dot to be be Java.)

	    // [IDEA] Eliminate package public/private distinction. Make import affect a reader
	    // (like Java). If you write a fully qualified name, you get it (always). Define default
	    // imports for a reader. Import functions should allow for import of all functions, or
	    // symbols with specified attributes. (selective-import <attributes> "org.foo.*")

	    // [IDEA] Implement a defpackage function or package manipulation functions. Define
	    // default imports for any reader that is in a package.

	    // [IDEA] Symbols could have attributes (like, public, protected, private) that affect
	    // things.
	    compileLetStar (mv, expression, valueType, allowNarrowing, liberalTruth);
	}
	else if (symbol.is ("cond"))
	{
	    if (valueType == null)
	    {
		compileVoidCond (mv, expression);
	    }
	    else if (valueType.equals (boolean.class))
	    {
		compileBooleanCond (mv, expression);
	    }
	    else
	    {
		compileCond (mv, expression, valueType, allowNarrowing, liberalTruth);
	    }
	}
	else if (symbol.is ("the"))
	{
	    compileThe (mv, expression, valueType, allowNarrowing, liberalTruth);
	}
	else
	{
	    throw new IllegalArgumentException ("NYI special form " + symbol);
	}
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

    // private void compileQuote (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo () (quote bar))
    // if (boolean.class.equals (valueType))
    // {
    // mv.visitLdcInsn (true);
    // }
    // else if (valueType != null)
    // {
    // final Symbol quote = (Symbol)e.get (0);
    // final Symbol reference = quote.gensym ();
    // final Object quoted = e.get (1);
    // addQuotedConstant (reference, quoted);
    // final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
    // // LOGGER.finer (new LogString ("Quoted reference to %s (%s)", typeDescriptor, quoted));
    // mv.visitVarInsn (ALOAD, 0);
    // final String classInternalName = shellClassType.getInternalName ();
    // mv.visitFieldInsn (GETFIELD, classInternalName, reference.getName (), typeDescriptor);
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }
    // }

    // private void compileProgn (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo () (progn (printf "a%n") (printf "b%n") 3))
    // if (e.size () == 0)
    // {
    // pushDefaultValue (mv, valueType);
    // }
    // else
    // {
    // for (int i = 1; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null);
    // }
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // }
    // }

    // private void compileIf (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType, final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // // (define foo (x) (if x 1 2))
    // // (define foo (x) (if x 1 (printf "a%n") (printf "b%n") 3))
    //
    // compileExpression (mv, e.get (1), boolean.class, false, true);
    // final Label l1 = new Label ();
    // final Label l2 = new Label ();
    // mv.visitJumpInsn (IFEQ, l2);
    //
    // // True case
    // compileExpression (mv, e.get (2), valueType, allowNarrowing, liberalTruth);
    // mv.visitJumpInsn (GOTO, l1);
    //
    // // False case. Nothing on the stack.
    // mv.visitLabel (l2);
    // for (int i = 3; i < e.size () - 1; i++)
    // {
    // // valueType null means nothing is left on the stack
    // compileExpression (mv, e.get (i), null);
    // }
    // if (e.size () <= 3)
    // {
    // // No false case so return default
    // pushDefaultValue (mv, valueType);
    // }
    // else
    // {
    // // Return the last value
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // }
    //
    // // Jump here after true case or fall through after else.
    // // Return final value.
    // mv.visitLabel (l1);
    // }

    // /** Compile an 'and' expression whose value will be ignored. */
    // private void compileVoidAnd (final GeneratorAdapter mv, final LispList e)
    // {
    // // (define foo (a b) (and) 1)
    // // (define foo (a b) (and a b) 2)
    // if (e.size () > 0)
    // {
    // final Label l1 = new Label ();
    // for (int i = 1; i < e.size (); i++)
    // {
    // compileExpression (mv, e.get (i), boolean.class, false, true);
    // mv.visitJumpInsn (IFEQ, l1);
    // }
    // mv.visitLabel (l1);
    // }
    // }
    //
    // /** Compile an 'and' expression whose value is only used as a boolean */
    // private void compileBooleanAnd (final GeneratorAdapter mv, final LispList e)
    // {
    // // (define foo (a b) (if (and a b) 1 2))
    // final Label l1 = new Label ();
    // for (int i = 1; i < e.size (); i++)
    // {
    // compileExpression (mv, e.get (i), boolean.class, false, true);
    // mv.visitJumpInsn (IFEQ, l1);
    // }
    // // True case
    // final Label l2 = new Label ();
    // mv.visitInsn (ICONST_1);
    // mv.visitJumpInsn (GOTO, l2);
    //
    // // False case
    // mv.visitLabel (l1);
    // mv.visitInsn (ICONST_0);
    //
    // // Jump here after true case or fall through after false.
    // // Return final value.
    // mv.visitLabel (l2);
    // }
    //
    // private void compileAnd (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType, final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // // (define foo (a b) (and))
    // // (define foo (a b) (and a b))
    // final Label l1 = new Label ();
    // final Label l2 = new Label ();
    // mv.visitInsn (ICONST_1);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;",
    // false);
    // for (int i = 1; i < e.size (); i++)
    // {
    // mv.visitInsn (POP);
    // compileExpression (mv, e.get (i), Object.class /* TODO */, false, true);
    // mv.visitInsn (DUP);
    // final Label l3 = new Label ();
    // mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
    // mv.visitJumpInsn (IFEQ, l3);
    // mv.visitInsn (DUP);
    // mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
    // mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
    // mv.visitJumpInsn (IFEQ, l1);
    // mv.visitLabel (l3);
    // }
    // // True case
    // mv.visitJumpInsn (GOTO, l2);
    //
    // // False case
    // mv.visitLabel (l1);
    // mv.visitInsn (POP);
    // mv.visitInsn (ICONST_0);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;",
    // false);
    //
    // // Jump here after true case or fall through after false.
    // // Return final value.
    // mv.visitLabel (l2);
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }

    // /** Compile an 'or' expression whose value will be ignored. */
    // private void compileVoidOr (final GeneratorAdapter mv, final LispList e)
    // {
    // // (define foo (a b) (or) 2)
    // // (define foo (a b) (or a b) 3)
    // if (e.size () > 0)
    // {
    // final Label l1 = new Label ();
    // for (int i = 1; i < e.size (); i++)
    // {
    // compileExpression (mv, e.get (i), boolean.class, false, true);
    // mv.visitJumpInsn (IFNE, l1);
    // }
    // mv.visitLabel (l1);
    // }
    // }
    //
    // /** Compile an 'or' expression whose value is only used as a boolean */
    // private void compileBooleanOr (final GeneratorAdapter mv, final LispList e)
    // {
    // // (define foo (a b) (if (or a b) 1 2))
    // final Label l1 = new Label ();
    // for (int i = 1; i < e.size (); i++)
    // {
    // compileExpression (mv, e.get (i), boolean.class, false, true);
    // mv.visitJumpInsn (IFNE, l1);
    // }
    // // False case
    // final Label l2 = new Label ();
    // mv.visitInsn (ICONST_0);
    // mv.visitJumpInsn (GOTO, l2);
    //
    // // True case
    // mv.visitLabel (l1);
    // mv.visitInsn (ICONST_1);
    //
    // // Jump here after false case or fall through after true.
    // // Return final value.
    // mv.visitLabel (l2);
    // }
    //
    // private void compileOr (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType, final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // // (define foo (a b) (or))
    // // (foo 1 2)
    // // (define foo (a b) (or a b))
    // final Label l1 = new Label ();
    // mv.visitInsn (ICONST_0);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;",
    // false);
    // for (int i = 1; i < e.size (); i++)
    // {
    // mv.visitInsn (POP);
    // compileExpression (mv, e.get (i), Object.class /* TODO */, false, true);
    // mv.visitInsn (DUP);
    // mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
    // mv.visitJumpInsn (IFEQ, l1);
    // mv.visitInsn (DUP);
    // mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
    // mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
    // mv.visitJumpInsn (IFNE, l1);
    // }
    // // False case
    // mv.visitInsn (POP);
    // mv.visitInsn (ICONST_0);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;",
    // false);
    //
    // // Jump here for true case or fall through in false case
    // mv.visitLabel (l1);
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }

    // private void compileWhen (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType, final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // // (define foo (x) (when x 1 2))
    // // (define foo (x) (when x 1 (printf "a%n") (printf "b%n") 3))
    //
    // final Label l1 = new Label ();
    // final Label l2 = new Label ();
    // compileExpression (mv, e.get (1), boolean.class, false, true);
    // mv.visitJumpInsn (IFEQ, l2);
    //
    // // True case
    // for (int i = 2; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null);
    // }
    // // Don't pop the last value
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // mv.visitJumpInsn (GOTO, l1);
    //
    // // False case.
    // mv.visitLabel (l2);
    // pushDefaultValue (mv, valueType, false);
    //
    // // Jump here after true case or fall through after else.
    // // Return final value.
    // mv.visitLabel (l1);
    // }

    // private void compileUnless (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo (x) (unless x 1 2))
    // // (define foo (x) (unless x 1 (printf "a%n") (printf "b%n") 3))
    // compileExpression (mv, e.get (1), boolean.class, false, true);
    // final Label l1 = new Label ();
    // mv.visitJumpInsn (IFNE, l1);
    //
    // // True case
    // for (int i = 2; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null);
    // }
    // // Don't pop the last value
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // final Label l3 = new Label ();
    // mv.visitJumpInsn (GOTO, l3);
    //
    // // False case where we must pop a value
    // mv.visitLabel (l1);
    // pushDefaultValue (mv, valueType, true);
    //
    // // Jump here after true case or fall through after else.
    // // Return final value.
    // mv.visitLabel (l3);
    // }

    // private void compileSetq (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType, final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // // (define foo (x) (setq x 3))
    // // (define foo (x) (setq x 3) x)
    // // (define foo (x) (setq a x))
    // // (define foo (x) (let ((a 3)) (setq a (+ a x)) a))
    // final Symbol symbol = (Symbol)e.get (1);
    // if (methodArgs.contains (symbol))
    // {
    // // Parameter reference
    // // [TODO] If we can determine the type, use that information.
    // final int argIndex = methodArgs.indexOf (symbol);
    // final Class<?> argClass = methodArgClasses.get (argIndex);
    // LOGGER.finer (new LogString ("Setq parameter %s (%d)", symbol, argIndex));
    // compileArgSetq (mv, argClass, argIndex, e.get (2), valueType, allowNarrowing, liberalTruth);
    // }
    // else if (localVariableMap.containsKey (symbol))
    // {
    // final LocalBinding lb = localVariableMap.get (symbol);
    // LOGGER.finer (new LogString ("Setq local %s (%d)", symbol, lb));
    // compileLocalSetq (mv, lb.getClass (), lb.getLocalRef (), e.get (2), valueType,
    // allowNarrowing, liberalTruth);
    // }
    // else
    // {
    // if (!symbolReferences.contains (symbol))
    // {
    // symbolReferences.add (symbol);
    // }
    // LOGGER.finer (new LogString ("Symbol assignment to %s", symbol));
    // // [TODO] If the symbol valueCell is constant, use the current value.
    // // [TODO] If the valueCell is a TypedValueCell, use the type information.
    // mv.visitVarInsn (ALOAD, 0);
    // final String classInternalName = shellClassType.getInternalName ();
    // mv.visitFieldInsn (GETFIELD, classInternalName, createJavaSymbolName (symbol),
    // "Llisp/Symbol;");
    // compileExpression (mv, e.get (2), Object.class /* TODO */);
    // if (valueType != null)
    // {
    // // Copy the expression value so it becomes the return value
    // mv.visitInsn (DUP_X1);
    // }
    //
    // mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V",
    // false);
    // // Return the expression value
    // if (!globalReferences.contains (symbol))
    // {
    // globalReferences.add (symbol);
    // LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
    // }
    // if (valueType != null)
    // {
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }
    // }
    // }
    //
    // private void compileArgSetq (final GeneratorAdapter mv, final Class<?> varClass, final int
    // localRef, final Object expr,
    // final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // if (valueType == null)
    // {
    // compileExpression (mv, expr, varClass);
    // mv.storeArg (localRef);
    // }
    // else
    // {
    // compileExpression (mv, expr, varClass);
    // mv.visitInsn (DUP);
    // mv.storeArg (localRef);
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }
    // }
    //
    // private void compileLocalSetq (final GeneratorAdapter mv, final Class<?> varClass, final int
    // localRef, final Object expr,
    // final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // if (valueType == null)
    // {
    // compileExpression (mv, expr, varClass);
    // mv.storeLocal (localRef);
    // // mv.visitVarInsn (ASTORE, localRef);
    // }
    // else
    // {
    // compileExpression (mv, expr, varClass);
    // mv.visitInsn (DUP);
    // mv.storeLocal (localRef);
    // // mv.visitVarInsn (ASTORE, localRef);
    // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }
    // }

    // private void compileRepeat (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo (x) (repeat x 3))
    // // (define foo (x) (repeat x 3) 5)
    // // (define foo (x) (printf "bar%n"))
    // // (define foo (x) (repeat x (printf "bar%n")))
    // // (define foo (x) (repeat x (printf "bar%n")) 5)
    // // (define foo (x) (repeat x (not true)))
    // // (define foo (x) (repeat x true))
    // // (define foo (x) (repeat x (abs 5)))
    // // (define foo (x) (repeat x (printf "bar %s%n" x)))
    // // (define foo (x) (repeat x (setq a (+ a 1))))
    // // (define foo (x) (repeat (+ x 5) (setq a (+ a 1))))
    // // (define foo (x y) (repeat (+ x 5) (setq a (+ a y))))
    // // (define foo (x) (repeat x (printf "foo")) 5)
    // // (define int:foo () (repeat 1000 3))
    //
    // // Make a local variable for repeat count
    // final int countRef = mv.newLocal (Type.getType (int.class));
    //
    // // Compute repeat count
    // compileExpression (mv, e.get (1), int.class, false, false);
    // // Put repeat count number into local variable
    // mv.visitVarInsn (ISTORE, countRef);
    //
    // // Push default return value onto the stack
    // pushDefaultValue (mv, valueType);
    //
    // // Push iteration number onto the stack
    // mv.visitInsn (ICONST_0);
    //
    // // Jump to termination test
    // final Label l1 = new Label ();
    // mv.visitJumpInsn (GOTO, l1);
    //
    // // Start of iteration body
    // final Label l2 = new Label ();
    // mv.visitLabel (l2);
    // // Stack: iteration, value
    //
    // // <body code goes here>
    // if (valueType != null)
    // {
    // mv.visitInsn (SWAP);
    // }
    // // Stack: value, iteration
    // if (e.size () > 2)
    // {
    // if (valueType != null)
    // {
    // mv.visitInsn (POP);
    // }
    // for (int i = 2; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null, false, false);
    // }
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // }
    // if (valueType != null)
    // {
    // mv.visitInsn (SWAP);
    // }
    //
    // // Loop increment
    // // Stack: iteration, value
    // mv.visitInsn (ICONST_1);
    // mv.visitInsn (IADD);
    //
    // // Termination test
    // // Stack: iteration, value
    // mv.visitLabel (l1);
    //
    // mv.visitInsn (DUP);
    // mv.visitVarInsn (ILOAD, countRef);
    // // Stack: count, iteration, iteration, value
    // mv.visitJumpInsn (IF_ICMPLT, l2);
    // // Stack: iteration, value
    // // Remove iteration count
    // mv.visitInsn (POP);
    // // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // }

    // private void compileDotimes (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo () (dotimes (i 3) 0))
    // // (define foo () (dotimes (i 3) 0) 5)
    // // (define foo (n) (dotimes (i n) 0))
    // // (define foo () (dotimes (i 3) (printf "i = %s %n" i)))
    // // (define foo (n) (dotimes (i n) (printf "i = %s %n" i)))
    //
    // // Compute repeat count
    // final List<?> control = (List<?>)e.get (1);
    // final Object count = control.get (1);
    // // [TODO] Currently always compiles for Object result. Need to analyze the body and
    // // determine if the value is actually referenced.
    // compileExpression (mv, count, Object.class /* TODO */, false, false);
    // mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
    // mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
    // // Leave repeat count on the stack
    //
    // // Push default return value onto the stack
    // pushDefaultValue (mv, valueType);
    // // Stack [returnValue], repeatCount
    //
    // // Put iteration number into local variable
    // final Map<Symbol, LocalBinding> savedLocalVariableMap = localVariableMap;
    // localVariableMap = new LinkedHashMap<Symbol, LocalBinding> (localVariableMap);
    // // Create a local variable to hold the iteration number.
    // // This is always stored in boxed format so body code can reference it.
    // // Should be able to store this as an int if the body code can use it that way.
    // final Type type = Boxer.INTEGER_TYPE;
    // final int iterationRef = mv.newLocal (type);
    // final Symbol var = (Symbol)control.get (0);
    // final LocalBinding lb = new LocalBinding (var, type, iterationRef);
    // localVariableMap.put (var, lb);
    // mv.visitInsn (ICONST_0);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;",
    // false);
    // mv.visitVarInsn (ASTORE, iterationRef);
    //
    // // Jump to termination test
    // final Label l1 = new Label ();
    // mv.visitJumpInsn (GOTO, l1);
    //
    // // Start of iteration body
    // final Label l2 = new Label ();
    // mv.visitLabel (l2);
    // // Stack repeatCount, [returnValue]
    // if (valueType != null)
    // {
    // mv.visitInsn (SWAP); // Save repeat count
    // }
    // // <body code goes here>
    // for (int i = 2; i < e.size (); i++)
    // {
    // if (valueType != null)
    // {
    // mv.visitInsn (POP);
    // }
    // compileExpression (mv, e.get (i), valueType);
    // }
    //
    // // Loop increment
    // mv.visitVarInsn (ALOAD, iterationRef);
    // mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
    // mv.visitInsn (ICONST_1);
    // mv.visitInsn (IADD);
    // mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;",
    // false);
    // mv.visitVarInsn (ASTORE, iterationRef);
    //
    // // // Termination test
    // mv.visitLabel (l1);
    // // Stack [returnValue], repeatCount
    //
    // if (valueType != null) // ***ADDED
    // {
    // mv.visitInsn (SWAP);
    // }
    // // Stack repeatCount, [returnValue]
    // mv.visitInsn (DUP); // Dup count
    // // Stack repeatCount, repeatCount, [returnValue]
    // mv.visitVarInsn (ALOAD, iterationRef);
    // // Stack iteration, repeatCount, repeatCount, [returnValue]
    // mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
    // mv.visitJumpInsn (IF_ICMPGT, l2);
    // // Stack repeatCount, [returnValue]
    //
    // mv.visitInsn (POP); // Remove repeat count
    // // coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    // // Return last body value
    // localVariableMap = savedLocalVariableMap;
    // }

    // private void compileWhile (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo (x) (setq a 0) (while (< a x) (printf "A: %s%n" a) (setq a (+ a 1))))
    //
    // // Load default value
    // pushDefaultValue (mv, valueType);
    //
    // // Perform iteration test
    // final Label l1 = new Label ();
    // mv.visitLabel (l1);
    // final Label l2 = new Label ();
    // compileExpression (mv, e.get (1), boolean.class, false, true);
    // mv.visitJumpInsn (IFEQ, l2);
    //
    // // Loop body
    // if (valueType != null)
    // {
    // mv.visitInsn (POP);
    // }
    // for (int i = 2; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null);
    // }
    // // Don't pop the last value
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // mv.visitJumpInsn (GOTO, l1);
    //
    // mv.visitLabel (l2);
    // }

    // private void compileUntil (final GeneratorAdapter mv, final LispList e, final Class<?>
    // valueType,
    // final boolean allowNarrowing, final boolean liberalTruth)
    // {
    // // (define foo (x) (setq a 0) (until (> a x) (printf "A: %s%n" a) (setq a (+ a 1))))
    //
    // // Load default value
    // pushDefaultValue (mv, valueType);
    //
    // // Perform iteration test
    // final Label l1 = new Label ();
    // mv.visitLabel (l1);
    // final Label l2 = new Label ();
    // compileExpression (mv, e.get (1), boolean.class, false, true);
    // mv.visitJumpInsn (IFNE, l2);
    //
    // // Loop body
    // if (valueType != null)
    // {
    // mv.visitInsn (POP);
    // }
    // for (int i = 2; i < e.size () - 1; i++)
    // {
    // compileExpression (mv, e.get (i), null);
    // }
    // // Don't pop the last value
    // compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
    // mv.visitJumpInsn (GOTO, l1);
    //
    // mv.visitLabel (l2);
    // }

    private void compileLet (final GeneratorAdapter mv, final LispList e, final Class<?> valueType, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	// (define foo (x) (let ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let ((a 1) (b 2)) a))
	// (define foo () (let ((a b) (b a)) a))
	// (define foo (x) (let ((a b) (b a)) (if x a b)))

	// Compile expression values onto the stack in order
	final LispList args = (LispList)e.get (1);
	final Map<Symbol, LocalBinding> newLocalVariableMap = new LinkedHashMap<Symbol, LocalBinding> (localVariableMap);
	final Map<Symbol, LocalBinding> savedLocalVariableMap = localVariableMap;
	for (int i = 0; i < args.size (); i++)
	{
	    final Object clause = args.get (i);
	    final LispList c = (LispList)clause;
	    final Object varSpec = c.get (0);
	    final Symbol var = CompileSupport.getNameVariable (varSpec);
	    final Class<?> varClass = CompileSupport.getNameType (varSpec);
	    final Type varType = Type.getType (varClass);
	    final int localRef = mv.newLocal (varType);
	    compileExpression (mv, c.get (1), varClass);
	    mv.storeLocal (localRef);
	    final LocalBinding lb = new LocalBinding (var, varType, localRef);
	    newLocalVariableMap.put (var, lb);
	}
	localVariableMap = newLocalVariableMap;
	// Evaluate optional body forms
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Evaluate last (required) body form
	compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
	// coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    }

    private void compileLetStar (final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (let* ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let* ((a 1) (b 2)) a))
	// (define foo () (let* ((a b) (b a)) b))
	// (define bar () (let ((a b) (b a)) b))
	// (define foo (x) (let* ((a b) (b a)) (if x a b)))
	// final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;

	// Compile expression values onto the stack in order
	// Bind the variables as each value is computed
	final Map<Symbol, LocalBinding> savedLocalVariableMap = localVariableMap;
	localVariableMap = new LinkedHashMap<Symbol, LocalBinding> (localVariableMap);
	final LispList args = (LispList)e.get (1);
	for (final Object clause : args)
	{
	    final LispList c = (LispList)clause;
	    final Object varSpec = c.get (0);
	    final Symbol var = CompileSupport.getNameVariable (varSpec);
	    final Class<?> varClass = CompileSupport.getNameType (varSpec);
	    final Type varType = Type.getType (varClass);
	    compileExpression (mv, c.get (1), varClass);
	    final int localRef = mv.newLocal (varType);
	    mv.storeLocal (localRef);
	    final LocalBinding lb = new LocalBinding (var, varType, localRef);
	    localVariableMap.put (var, lb);
	}

	// Evaluate optional body forms
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Evaluate last (required) body form
	compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);

	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
	// coerceRequired (mv, valueType, allowNarrowing, liberalTruth);
    }

    /** Compile a cond expression where the value will be used. */
    private void compileCond (final GeneratorAdapter mv, final LispList e, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (setq showBytecode t)
	// (define foo (x) (cond (x 1)))
	// (define foo (x) (cond ((= x 1) 'alpha)))
	// (define foo (x) (cond ((= x 1) 'alpha) ((= x 2) 'beta) ((= x 3) 'gamma) (true 'delta)))
	if (valueClass == null)
	{
	    throw new Error ("Use compileVoidCond when valueType is null");
	}
	final Package system = PackageFactory.getSystemPackage ();
	final Symbol var = system.internSymbol ("result").gensym ();

	// Setup return value in a local variable

	// Store as an object until return time
	final int resultRef = mv.newLocal (Boxer.OBJECT_TYPE);
	final LocalBinding lb = new LocalBinding (var, Boxer.OBJECT_TYPE, resultRef);
	final Map<Symbol, LocalBinding> savedBindings = localVariableMap;
	localVariableMap = new HashMap<Symbol, LocalBinding> (localVariableMap);
	localVariableMap.put (var, lb);
	pushDefaultValue (mv, Object.class);
	mv.storeLocal (resultRef);

	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    // Stack is empty
	    final LispList clause = (LispList)e.get (i);
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();
	    final Label l3 = new Label ();
	    final Label l4 = new Label ();
	    if (clause.size () == 1)
	    {
		compileExpression (mv, key, Object.class, false, true);
		mv.visitInsn (DUP);
		mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
		mv.visitJumpInsn (IFEQ, l2); // Check for boolean

		mv.visitInsn (DUP);
		mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
		mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
		mv.visitJumpInsn (IFEQ, l3); // Proceed to next clause

		// Clause selected and value of key expression is return value
		mv.visitLabel (l2);
	    }
	    else
	    {
		// No need to save value of key expression
		compileExpression (mv, key, boolean.class, false, true);
		mv.visitJumpInsn (IFEQ, l4); // Proceed to next clause

		// Clause selected
		mv.visitLabel (l2);

		// One entry on stack
		if (clause.size () > 1)
		{
		    for (int j = 1; j < clause.size () - 1; j++)
		    {
			compileExpression (mv, clause.get (j), null, false, false);
		    }
		    compileExpression (mv, clause.last (), valueClass, allowNarrowing, liberalTruth);
		}
	    }

	    mv.storeLocal (resultRef);
	    // Stack is empty
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l3);
	    mv.visitInsn (POP);
	    mv.visitLabel (l4);
	    // Stack is empty
	}
	// Return result
	mv.visitLabel (l1);
	mv.loadLocal (resultRef);
	coerceRequired (mv, valueClass, allowNarrowing, liberalTruth);
	localVariableMap = savedBindings;
    }

    /** Case where no return value is required. */
    private void compileVoidCond (final GeneratorAdapter mv, final LispList e)
    {
	// (define foo (x) (cond ((= x 1) (printf "one%n")) ((= x 2)(printf "two%n"))) 'done)

	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final int size = clause.size ();
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();

	    compileExpression (mv, key, boolean.class);
	    mv.visitJumpInsn (IFEQ, l2);

	    // Clause selected
	    for (int j = 1; j < size; j++)
	    {
		compileExpression (mv, clause.get (j), null);
	    }
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l2);
	}
	// Return result
	mv.visitLabel (l1);
    }

    /** Case where only a boolean value is required. */
    // (define foo (x) (when (cond ((= x 1)) ((= x 2) false) ((= x 3) true)) (printf "when%n")))
    // (define foo (x) (when (cond ((= x 1)) ((= x 2) false) ((= x 3))) (printf "when%n")))
    private void compileBooleanCond (final GeneratorAdapter mv, final LispList e)
    {
	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final int size = clause.size ();
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();

	    compileExpression (mv, key, boolean.class);
	    mv.visitJumpInsn (IFEQ, l2);

	    // Clause selected
	    if (size > 1)
	    {
		for (int j = 1; j < size - 1; j++)
		{
		    compileExpression (mv, clause.get (j), null);
		}
		compileExpression (mv, clause.get (size - 1), boolean.class);
	    }
	    else
	    {
		mv.visitLdcInsn (true);
	    }
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l2);
	}
	// Return result
	mv.visitLdcInsn (false);
	mv.visitLabel (l1);
    }

    private void compileThe (final GeneratorAdapter mv, final LispList e, final Class<?> valueClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	final Object type = e.get (1);
	final Object arg = e.get (2);

	compileThe (mv, type, arg, valueClass, allowNarrowing, liberalTruth);
    }

    private void compileThe (final GeneratorAdapter mv, final Object type, final Object arg, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (setq system.showBytecode t)
	// (define foo () byte:3)
	// [TODO] Need to allow narrowing conversions here
	if (type instanceof Symbol)
	{
	    final Symbol t = (Symbol)type;
	    if (t.is ("byte"))
	    {
		// (define byte:foo () (the byte int:3))
		// (define byte:foo () byte:int:3)
		// (d (foo))
		compileExpression (mv, arg, byte.class, true, false);
		convert.convert (mv, byte.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("char"))
	    {
		compileExpression (mv, arg, int.class, true, false);
		// mv.visitInsn (I2C); // Narrow
		convert.convert (mv, char.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("short"))
	    {
		// (define short:foo () (the short int:3))
		compileExpression (mv, arg, int.class, true, false);
		convert.convert (mv, short.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("int"))
	    {
		compileExpression (mv, arg, int.class, true, false);
		convert.convert (mv, int.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("long"))
	    {
		// (define long:foo () int:3) ; Widening
		// (define long:foo () float:3.3) ; Error needs cast
		// (define long:foo () long:float:3.3) ; Explicit cast
		compileExpression (mv, arg, long.class, true, false);
		convert.convert (mv, long.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("float"))
	    {
		// NOT WORKING
		// mv.visitLdcInsn ((float)6.9);
		compileExpression (mv, arg, float.class, true, false);
		convert.convert (mv, float.class, valueClass, true, false);
		return;
	    }
	    if (t.is ("double"))
	    {
		compileExpression (mv, arg, double.class, true, false);
		convert.convert (mv, double.class, valueClass, true, false);
		return;
	    }
	    compileThe (mv, t.getName (), arg, valueClass, allowNarrowing, liberalTruth);
	    return;
	}
	// Narrowing has not been implemented below this line
	else if (type instanceof Class)
	{
	    final Class<?> c = (Class<?>)type;
	    convert.convert (mv, Object.class, c, true, false);
	    convert.convert (mv, c, valueClass, false, false);
	    return;
	}
	if (type instanceof String)
	{
	    final String t = (String)type;
	    try
	    {
		final Class<?> c = Class.forName (t);
		convert.convert (mv, c, valueClass, false, false);
		return;
	    }
	    catch (final ClassNotFoundException e)
	    {
	    }
	    if (t.indexOf (".") < 0)
	    {
		try
		{
		    final Class<?> c = Class.forName ("java.lang." + t);
		    convert.convert (mv, c, valueClass, false, false);
		    return;
		}
		catch (final ClassNotFoundException e)
		{
		}
	    }
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
     * Push a default value onto the stack. If the value will be a primitive boolean, use false as
     * the default value.
     *
     * @param mv GeneratorAdapter to produce code.
     * @param valueClass The value type to return.
     */
    private void pushDefaultValue (final GeneratorAdapter mv, final Class<?> valueClass)
    {
	pushDefaultValue (mv, valueClass, false);
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
