
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
    private static ConstantExpression constantExpression = new ConstantExpression ();

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

    /**
     * Map from reference name to quoted Object. This is created in the CompileLoader and saved
     * here. When it is modified, the class can get at it by getClassLoader().getQuotedReferences
     * ();
     */
    private final Map<String, Object> quotedReferences;

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
	quotedReferences = quotedReferencesMap;
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

    /** Get the current local binding context. */
    @Override
    public Map<Symbol, LocalBinding> getLocalBindingContext ()
    {
	return localVariableMap;
    }

    /** Set the current local binding context. */
    @Override
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

    /**
     * Keep track of a symbol that has a global reference. This is only used to produce a log
     * message. globalReferences does nothing else.
     */
    @Override
    public void addGlobalReference (final Symbol symbol)
    {
	if (!globalReferences.contains (symbol))
	{
	    globalReferences.add (symbol);
	    LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
	}
    }

    @Override
    public void addQuotedConstant (final Symbol reference, final Object quoted)
    {
	quotedReferences.put (reference.getName (), quoted);
    }

    /**
     * Push a default value onto the stack.
     *
     * @param mv GeneratorAdapter to produce code.
     * @param valueClass The value type to return.
     * @param booleanDefault If the value will be a primitive boolean, use this as the default
     *            value.
     */
    @Override
    public void pushDefaultValue (final GeneratorAdapter mv, final Class<?> valueClass, final boolean booleanDefault)
    {
	convert.pushDefaultValue (mv, valueClass, booleanDefault);
    }

    /** Convert the result of an expression to the type required by the called. */
    public void convertResultType (final GeneratorAdapter mv, final Class<?> actualClass, final Class<?> requiredClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (actualClass.equals (void.class))
	{
	    if (requiredClass != null)
	    {
		pushDefaultValue (mv, requiredClass, false);
	    }
	}
	else if (!actualClass.equals (requiredClass))
	{
	    convert.convert (mv, actualClass, requiredClass, allowNarrowing, liberalTruth);
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
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    // LOGGER.finer (new LogString ("Field: private Quoted %s; [%s]", reference, quoted));
	    createField (ACC_PRIVATE, reference, typeDescriptor);
	}
	// Create init method as the very last step, so all requirements from other compilation
	// steps are known
	createInitI ();
	cv.visitEnd ();
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
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitInsn (DUP);
	    mv.visitMethodInsn (INVOKEVIRTUAL, objectType.getInternalName (), "getClass", Type.getMethodDescriptor (classType),
	            false);
	    mv.visitMethodInsn (INVOKEVIRTUAL, classType.getInternalName (), "getClassLoader", "()Ljava/lang/ClassLoader;",
	            false);
	    mv.visitTypeInsn (CHECKCAST, classLoaderInternalName);
	    mv.visitMethodInsn (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;", false);

	    mv.visitLdcInsn (reference);
	    mv.visitMethodInsn (INVOKEINTERFACE, "java/util/Map", "get", Type.getMethodDescriptor (objectType, objectType), true);
	    final Type quotedType = Type.getType (quoted.getClass ());
	    final String typeDescriptor = quotedType.getDescriptor ();
	    mv.visitTypeInsn (CHECKCAST, quotedType.getInternalName ());
	    mv.visitFieldInsn (PUTFIELD, classInternalName, reference, typeDescriptor);
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
     * @param valueClass Class of value to leave on the stack.
     * @param allowNarrowing When true, narrowing conversions will be generated if required.
     *            Otherwise narrowing throws and error.
     * @param liberalTruth When set, any non-boolean result is accepted as true. Otherwise, boolean
     *            testing requires strictly boolean values.
     */
    @Override
    public void compileExpression (final GeneratorAdapter mv, final Object e, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (e == null)
	{
	    throw new Error ("Null is an illegal expression");
	}
	else if (e instanceof LispList)
	{
	    compileFunctionCall (mv, (LispList)e, valueClass, allowNarrowing, liberalTruth);
	}
	else if (valueClass != null)
	{
	    if (e instanceof Symbol)
	    {
		final Symbol symbol = (Symbol)e;
		compileSymbolReference (mv, symbol, valueClass, allowNarrowing, liberalTruth);
	    }
	    else
	    {
		constantExpression.compileConstantExpression (mv, e, valueClass, allowNarrowing, liberalTruth);
	    }
	}
    }

    /**
     * Compile an expression to calculate the value of a symbol. This determines if the symbol is an
     * argument, local variable or global symbol and calculates the correct value.
     *
     * @param mv The bytecode generator.
     * @param symbol The symbol value to calculate.
     * @param valueClass The class that the output value needs to be converted to. When this is
     *            called the valueClass null has already been eliminated.
     * @param allowNarrowing Should output value conversion be allowed to make narrowing conversions
     *            or should an error be produced instead. If the determination can be made at
     *            compile time, it will be, but this may require production of runtime code to check
     *            the value type.
     * @param liberalTruth Should non-boolean values be treated as the value 'true', or throw an
     *            error? This is significant when the valueClass is primitive boolean.
     */
    private void compileSymbolReference (final GeneratorAdapter mv, final Symbol symbol, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	if (methodArgs.contains (symbol))
	{
	    // Parameter reference
	    final int argIndex = methodArgs.indexOf (symbol);
	    mv.loadArg (argIndex);
	    final Class<?> fromClass = methodArgClasses.get (argIndex);
	    final Class<?> toClass = valueClass;
	    convert.convert (mv, fromClass, toClass, allowNarrowing, liberalTruth);
	}
	else if (localVariableMap.containsKey (symbol))
	{
	    // Reference to a local lexical variable
	    // [TODO] If we can determine the type, use that information.
	    final LocalBinding lb = localVariableMap.get (symbol);
	    final int localRef = lb.getLocalRef ();
	    mv.loadLocal (localRef);
	    final Class<?> fromClass = lb.getClass ();
	    final Class<?> toClass = valueClass;
	    convert.convert (mv, fromClass, toClass, allowNarrowing, liberalTruth);
	}
	else if (symbol.is ("true") && valueClass.equals (boolean.class))
	{
	    // Special case for global symbol "true"
	    mv.visitLdcInsn (true);
	}
	else if (symbol.is ("false") && valueClass.equals (boolean.class))
	{
	    // Special case for global symbol "false"
	    mv.visitLdcInsn (false);
	}
	else
	{
	    // Reference to a global variable
	    addGlobalReference (symbol); // Log message
	    addSymbolReference (symbol); // Make symbol available at execution time
	    // [TODO] If the symbol valueCell is constant, use the current value.
	    // [TODO] If the valueCell is a TypedValueCell, use the type information.
	    mv.visitVarInsn (ALOAD, 0);
	    final String classInternalName = shellClassType.getInternalName ();
	    mv.visitFieldInsn (GETFIELD, classInternalName, createJavaSymbolName (symbol), "Llisp/Symbol;");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false);
	    coerceRequired (mv, valueClass);
	}
    }

    private void compileFunctionCall (final GeneratorAdapter mv, final LispList expression, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// Need to be able to compile a call to an undefined function (i.e. recursive call)
	LOGGER.finer (new LogString ("Compile nested form %s", expression));
	if (expression.size () == 0)
	{
	    throw new Error ("Mal-formed function call");
	}
	// [TODO] Consider alternatives of pushing this code into the compiled function
	// vs making these choices at compile time. The compile time choice will produce
	// faster code, but it won't be able to change behavior of the called function
	// is changed. This whole block of functionality could be a private method in
	// the CompiledShell class. This would evaluate the arguments into a List/Array
	// and pass them in. The compiler should keep track of all the function
	// definitions it uses, and also keep the source expression. If any function
	// definition changes, the code should be recompiled.
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    // Look for a specialized compiler definition
	    final ObjectMethod compiler = function.getCompiler ();
	    if (compiler != null)
	    {
		// Some 'normal' functions need special coding, i.e, arithmetic and comparisons, so
		// this is called for any function with a compiler, not just special forms.
		compileSpecialFunctionCall (mv, expression, valueClass, allowNarrowing, liberalTruth);
		return;
	    }
	    if (function instanceof SpecialFunctionCell)
	    {
		// Since this is not a known special function, we are stuck and can't proceed
		throw new IllegalArgumentException ("Unrecognized special form " + symbol);
		// compileSpecialFunctionCall (mv, symbol, e, valueClass, allowNarrowing,
		// liberalTruth);
		// return;
	    }
	    if (function instanceof MacroFunctionCell)
	    {
		// Expand and replace
		final MacroFunctionCell macro = (MacroFunctionCell)function;
		Object replacement;
		try
		{
		    replacement = macro.expand (expression);
		}
		catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e1)
		{
		    throw new Error ("Error expanding macro " + symbol, e1);
		}
		compileExpression (mv, replacement, valueClass, allowNarrowing, liberalTruth);
		return;
	    }
	    // final ObjectMethod compiler = function.getCompiler ();
	    // if (compiler != null)
	    // {
	    // // [TODO] Some 'normal' functions need special coding, i.e, arithmetic and
	    // // comparisons.
	    // LOGGER.finer (new LogString ("Compiling optimized call to standard function %s",
	    // symbol));
	    // compileSpecialFunctionCall (mv, symbol, e, valueClass, allowNarrowing, liberalTruth);
	    // return;
	    // }
	    if (optimizeFunctionCall (expression))
	    {
		compileDirectFunctionCall (mv, expression, valueClass, allowNarrowing, liberalTruth);
		return;
	    }
	}

	// Produce a generic call to a standard function.
	compileGeneralFunctionCall (mv, expression, valueClass, allowNarrowing, liberalTruth);
    }

    private void compileSpecialFunctionCall (final GeneratorAdapter mv, final LispList expression, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
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
	// [done] Hookup definition of special function calls to DefineLisp annotation.
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	final ObjectMethod compiler = function.getCompiler ();
	if (compiler != null)
	{
	    final Object object = compiler.getObject ();
	    final Method method = compiler.getMethod ();
	    try
	    {
		method.invoke (object, this, mv, expression, valueClass, allowNarrowing, liberalTruth);
	    }
	    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
	    {
		throw new Error ("Internal compiler error", e);
	    }
	    return;
	}
	throw new IllegalArgumentException ("Unrecognized special form " + symbol);
    }

    private boolean optimizeFunctionCall (final LispList expression)
    {
	// [TODO] If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.
	// [TODO] If we know argument types of the function we are about to call we can try to
	// compile the expression more efficiently.
	final int argCount = expression.size () - 1;
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    final ObjectMethod objectMethod = function.selectMethod (argCount);
	    if (objectMethod != null && symbol.getPackage ().getName ().equals ("system"))
	    {
		// Only methods with Object parameters work right now.
		// coerceRequired will need to be improved before general method parameters are ok.
		if (// objectMethod.isObjectOnly () &&
		!objectMethod.isVarArgs ())
		{
		    return Symbol.test ("optimize", true);
		}
	    }
	}
	return false;
    }

    private void compileDirectFunctionCall (final GeneratorAdapter mv, final LispList expression, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (setq showBytecode t)
	// (define foo () (getDefaultPackage))
	// (define foo (x) (1+ x))
	// (define foo (x) (not x))
	// (define foo (a b) (rem a b))
	final Symbol symbol = expression.head ();
	final Label l1 = new Label ();
	mv.visitLineNumber (617, l1);
	mv.visitLabel (l1);
	final FunctionCell function = symbol.getFunction ();
	final int argCount = expression.size () - 1;
	final ObjectMethod objectMethod = function.selectMethod (argCount);
	final Object target = objectMethod.getObject ();
	final Method method = objectMethod.getMethod ();

	LOGGER.fine ("Direct call to " + symbol);
	final Symbol reference = symbol.gensym ();
	final String methodSignature = objectMethod.getSignature ();
	final Type objectType = Type.getType (target.getClass ());
	final String objectClassInternalName = objectType.getInternalName ();
	addQuotedConstant (reference, target);
	mv.visitVarInsn (ALOAD, 0);
	final String classInternalName = shellClassType.getInternalName ();
	mv.visitFieldInsn (GETFIELD, classInternalName, reference.getName (), objectType.getDescriptor ());
	// Compile arguments here
	final Class<?>[] params = method.getParameterTypes ();
	for (int i = 0; i < params.length; i++)
	{
	    // (define f (x) (incr x))
	    final Object arg = expression.get (i + 1);
	    final Class<?> argType = params[i];
	    // System.out.printf ("%s (%s : %s) %s %n", symbol, i, arg, argType);
	    compileExpression (mv, arg, argType, false, false);
	}
	mv.visitMethodInsn (INVOKEVIRTUAL, objectClassInternalName, method.getName (), methodSignature, false);
	final Class<?> methodValueClass = method.getReturnType ();
	convert.convert (mv, methodValueClass, valueClass, allowNarrowing, liberalTruth);
    }

    /**
     * @param allowNarrowing
     * @param liberalTruth
     */
    private void compileGeneralFunctionCall (final GeneratorAdapter mv, final LispList expression, final Class<?> valueClass,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	final Symbol symbol = expression.head ();
	// Get to the function symbol at runtime.
	addSymbolReference (symbol);
	LOGGER.finer (new LogString ("Function symbol reference to %s", symbol));
	mv.visitVarInsn (ALOAD, 0);
	final String classInternalName = shellClassType.getInternalName ();
	mv.visitFieldInsn (GETFIELD, classInternalName, createJavaSymbolName (symbol), "Llisp/Symbol;");

	// Get the FunctionCell from the function symbol.
	// The call to getDefaultHandlerFunction will return a DefaultHandler that tries to invoke
	// the java method on arg 1 if the function has not been given any other definition.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getDefaultHandlerFunction", "()Llisp/symbol/FunctionCell;", false);

	// Compile the arguments. Pass all the arguments as elements of a single array of Objects.
	final int argCount = expression.size () - 1;
	ldcGeneral (mv, argCount);
	mv.visitTypeInsn (ANEWARRAY, "java/lang/Object");
	for (int i = 0; i < argCount; i++)
	{
	    mv.visitInsn (DUP);
	    ldcGeneral (mv, i);
	    compileExpression (mv, expression.get (i + 1), Object.class, false, false);
	    mv.visitInsn (AASTORE);
	}

	// Call invoke on the method retrieved from the FunctionCell of the function symbol.
	// Assume the function will still be defined when we execute this code.
	// [TODO] Could define an applyVoid method to return no value.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;", false);
	if (valueClass == null)
	{
	    mv.visitInsn (POP);
	}
	else
	{
	    coerceRequired (mv, valueClass);
	}
    }

    /** Load an integer constant using the best bytecode when value is small enough. */
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

    /**
     * Convert an instance of a boxed wrapper class into the corresponding primitive type. This
     * should only be called when the stack must contain an instance of a wrapper type. Note: short
     * and byte are converted to int. See ByteCodeUtils if this is a problem.
     */
    @Override
    @Deprecated
    public void coerceRequired (final GeneratorAdapter mv, final Class<?> valueClass)
    {
	if (valueClass == null)
	{
	    // coerceRequired (mv, Type.VOID_TYPE, allowNarrowing, liberalTruth);
	    // mv.visitInsn (POP);
	    throw new Error ("Don't use coerceRequired here");
	}
	final Type valueType = Type.getType (valueClass);
	final int sort = valueType.getSort ();

	// if (valueType.equals (Type.BOOLEAN_TYPE))
	// {
	// // Treat anything except Boolean as true.
	// convert.coerceBoolean (mv);
	// }
	// else
	if (sort > Type.VOID && sort < Type.ARRAY)
	{
	    // Call unbox with the type that we want to end up with
	    mv.unbox (valueType);
	}
	// Leave object types alone
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
