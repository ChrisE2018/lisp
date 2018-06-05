
package lisp.cc4;

import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.*;
import lisp.Symbol;
import lisp.cc.*;
import lisp.util.LogString;

public class TreeCompiler extends ClassNode implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (TreeCompiler.class.getName ());
    private static final TreeBoxer boxer = new TreeBoxer ();
    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private final CompileLoader compileLoader;
    private final Type returnType;
    private final Class<?> methodReturnClass;
    private final String methodName;
    private final LispList methodBody;
    private final Map<String, Object> quotedReferences;

    private final List<Symbol> methodArgs = new ArrayList<Symbol> ();
    private final List<Class<?>> methodArgClasses = new ArrayList<Class<?>> ();
    private final List<Type> methodArgTypes = new ArrayList<Type> ();
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

    public TreeCompiler (final ClassVisitor cv, final CompileLoader compileLoader, final Class<?> methodReturnClass,
            final String methodName, final LispList methodArgs, final LispList methodBody)
    {
	// '(setq system.compilerVersion "V4")' "(setq system.showBytecode t)"
	// (define foo () 1)
	super (Opcodes.ASM5);
	this.cv = cv;

	this.compileLoader = compileLoader;
	this.methodReturnClass = methodReturnClass;
	this.methodName = methodName;
	this.methodBody = methodBody;
	quotedReferences = compileLoader.getQuotedReferences ();

	returnType = Type.getType (methodReturnClass);
	for (final Object arg : methodArgs)
	{
	    final Symbol variable = CompileSupport.getNameVariable (arg);
	    final Class<?> varClass = CompileSupport.getNameType (arg);
	    final Type varType = Type.getType (varClass);
	    this.methodArgs.add (variable);
	    methodArgClasses.add (varClass);
	    methodArgTypes.add (varType);
	}
    }

    public Type getClassType ()
    {
	return compileLoader.getClassType ();
    }

    /** Keep track of a symbol that needs to be available as a class field. */
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
    public void addGlobalReference (final Symbol symbol)
    {
	if (!globalReferences.contains (symbol))
	{
	    globalReferences.add (symbol);
	    LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
	}
    }

    public void addQuotedConstant (final Symbol reference, final Object quoted)
    {
	quotedReferences.put (reference.getName (), quoted);
    }

    public Symbol addQuotedConstant (final Object quoted)
    {
	final Symbol reference = QUOTE_SYMBOL.gensym ();
	quotedReferences.put (reference.getName (), quoted);
	return reference;
    }

    @Override
    public void visitEnd ()
    {
	// put your transformation code here
	final MethodNode userMethod = getCompiledMethod ();
	final MethodNode initMethod = getInitMethod ();

	// Create field definitions for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String typeDescriptor = Type.getType (symbol.getClass ()).getDescriptor ();
	    fields.add (new FieldNode (ACC_PRIVATE, createJavaSymbolName (symbol), typeDescriptor, null, null));
	}
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    fields.add (new FieldNode (ACC_PRIVATE, reference, typeDescriptor, null, null));
	}

	methods.add (initMethod);
	if (symbolReferences.size () > 0)
	{
	    final MethodNode symbolMethod = getGetSymbolMethod ();
	    methods.add (symbolMethod);
	}
	// if (globalReferences.size () > 0)
	// {
	// final MethodNode valueMethod = getGetSymbolValueMethod ();
	// methods.add (valueMethod);
	// }
	methods.add (userMethod);

	// Chain transformation
	accept (cv);
    }

    private MethodNode getInitMethod ()
    {
	final MethodNode mn = new MethodNode (ACC_PUBLIC, "<init>", "(I)V", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (ALOAD, 0));
	il.add (new MethodInsnNode (INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false));

	// Do other things here...
	final String classInternalName = getClassType ().getInternalName ();

	final Type objectType = Type.getType (Object.class);
	final Type symbolType = Type.getType (Symbol.class);
	final String symbolTypeDescriptor = symbolType.getDescriptor ();
	final Type stringType = Type.getType (String.class);
	final Type classLoaderType = Type.getType (CompileLoader.class);
	final String classLoaderInternalName = classLoaderType.getInternalName ();

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaName = createJavaSymbolName (symbol);
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new LdcInsnNode (symbol.getPackage ().getName ()));
	    il.add (new LdcInsnNode (symbol.getName ()));
	    il.add (new MethodInsnNode (INVOKESPECIAL, classInternalName, "getSymbol",
	            Type.getMethodDescriptor (symbolType, stringType, stringType), false));
	    il.add (new FieldInsnNode (PUTFIELD, classInternalName, javaName, symbolTypeDescriptor));

	    LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaName, symbol));
	}
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new InsnNode (DUP));
	    final Type classType = Type.getType (Class.class);
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, objectType.getInternalName (), "getClass",
	            Type.getMethodDescriptor (classType), false));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, classType.getInternalName (), "getClassLoader",
	            "()Ljava/lang/ClassLoader;", false));
	    il.add (new TypeInsnNode (CHECKCAST, classLoaderInternalName));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;",
	            false));

	    il.add (new LdcInsnNode (reference));
	    il.add (new MethodInsnNode (INVOKEINTERFACE, "java/util/Map", "get",
	            Type.getMethodDescriptor (objectType, objectType), true));
	    final Type quotedType = Type.getType (quoted.getClass ());
	    final String typeDescriptor = quotedType.getDescriptor ();
	    il.add (new TypeInsnNode (CHECKCAST, quotedType.getInternalName ()));
	    il.add (new FieldInsnNode (PUTFIELD, classInternalName, reference, typeDescriptor));
	}

	// Standard init method return
	il.add (new InsnNode (RETURN));
	mn.maxStack = 0;
	mn.maxLocals = 1;
	return mn;
    }

    /** Create a method to locate a Symbol at runtime. */
    private MethodNode getGetSymbolMethod ()
    {
	final MethodNode mn =
	    new MethodNode (ACC_PRIVATE, "getSymbol", "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (ALOAD, 1));
	il.add (new MethodInsnNode (INVOKESTATIC, "lisp/PackageFactory", "getPackage", "(Ljava/lang/String;)Llisp/Package;",
	        false));
	il.add (new VarInsnNode (ALOAD, 2));
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Package", "findSymbol", "(Ljava/lang/String;)Llisp/Symbol;", false));
	il.add (new InsnNode (ARETURN));

	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    // /** Create a method to get a Symbol value at runtime. */
    // private MethodNode getGetSymbolValueMethod ()
    // {
    // final MethodNode mn =
    // new MethodNode (ACC_PRIVATE, "getSymbolValue",
    // "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", null, null);
    // final InsnList il = mn.instructions;
    // il.add (new VarInsnNode (ALOAD, 1));
    // il.add (new MethodInsnNode (INVOKESTATIC, "lisp/PackageFactory", "getPackage",
    // "(Ljava/lang/String;)Llisp/Package;",
    // false));
    // il.add (new VarInsnNode (ALOAD, 2));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Package", "findSymbol",
    // "(Ljava/lang/String;)Llisp/Symbol;", false));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;",
    // false));
    // il.add (new InsnNode (ARETURN));
    //
    // mn.maxStack = 0;
    // mn.maxLocals = 0;
    // return mn;
    // }

    /* Create a method to implement the user function being defined. */
    private MethodNode getCompiledMethod ()
    {
	final MethodNode mn = new MethodNode (ACC_PUBLIC, methodName, getMethodSignature (), null, null);
	final InsnList il = mn.instructions;
	final Map<Symbol, LocalBinding> locals = new LinkedHashMap<Symbol, LocalBinding> ();
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    final Symbol arg = methodArgs.get (i);
	    locals.put (arg, new LocalBinding (arg, methodArgClasses.get (i), i + 1));
	}
	final TreeCompilerContext context = new TreeCompilerContext (this, il, locals);
	for (int i = 0; i < methodBody.size () - 1; i++)
	{
	    final Object expr = methodBody.get (i);
	    final CompileResultSet resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	final CompileResultSet resultClass = context.compile (methodBody.last (), true);
	// (define double:foo (int:n) 1 2 n)
	for (final CompileResult resultKind : resultClass.getResults ())
	{
	    final LabelNode label = resultKind.getLabel ();
	    context.add (label);
	    if (resultKind instanceof ExplicitCompileResult)
	    {
		context.convert (((ExplicitCompileResult)resultKind).getResultClass (), methodReturnClass, false, false);
	    }
	    else
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)resultKind;
		final Object x = icr.getValue ();
		if (validLdcInsnParam (x))
		{
		    il.add (new LdcInsnNode (x));
		    final Class<?> ec = x.getClass ();
		    final Class<?> p = boxer.getUnboxedClass (ec);
		    context.convert (p != null ? p : ec, methodReturnClass, false, false);
		}
		else
		{
		    final Symbol s = addQuotedConstant (x);
		    final Class<?> quotedClass = x.getClass ();
		    final String typeDescriptor = Type.getType (quotedClass).getDescriptor ();
		    il.add (new VarInsnNode (ALOAD, 0));
		    final String classInternalName = getClassType ().getInternalName ();
		    il.add (new FieldInsnNode (GETFIELD, classInternalName, s.getName (), typeDescriptor));
		    context.convert (quotedClass, methodReturnClass, false, false);
		}
	    }
	    il.add (new InsnNode (returnType.getOpcode (IRETURN)));
	}
	// Better not get here
	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    /**
     * Test for {@link Integer}, a {@link Float}, a {@link Long}, a {@link Double} or a
     * {@link String}. {@link Boolean} also works.
     */
    private boolean validLdcInsnParam (final Object x)
    {
	return x instanceof Boolean || x instanceof Integer || x instanceof Float || x instanceof Long || x instanceof Double
	       || x instanceof String;
    }

    // private MethodNode getCompiledMethodOLD ()
    // {
    // final MethodNode mn = new MethodNode (ACC_PUBLIC, methodName, getMethodSignature (), null,
    // null);
    // final InsnList il = mn.instructions;
    // final Map<Symbol, LocalBinding> locals = new LinkedHashMap<Symbol, LocalBinding> ();
    // for (int i = 0; i < methodBody.size () - 1; i++)
    // {
    // final Object expr = methodBody.get (i);
    // final Class<?> resultClass = compile (il, locals, expr, false);
    // converter.convert (il, resultClass, void.class, false, false);
    // }
    // final Class<?> resultClass = compile (il, locals, methodBody.last (), true);
    // // (define double:foo (int:n) 1 2 n)
    // converter.convert (il, resultClass, methodReturnClass, false, false);
    //
    // il.add (new InsnNode (returnType.getOpcode (IRETURN)));
    // mn.maxStack = 0;
    // mn.maxLocals = 0;
    // return mn;
    // }

    // private void convert (final InsnList il, final Class<?> fromClass, final Class<?> toClass,
    // final boolean allowNarrowing,
    // final boolean liberalTruth)
    // {
    // converter.convert (il, fromClass, toClass, allowNarrowing, liberalTruth);
    // }

    // /**
    // * This should work with a more general data structure than an InsnList. The data structure
    // * should allow for:
    // * <li>sequence of instructions</li>
    // * <li>if <then> <else></li>
    // * <li>Implicit recording of current value</li>
    // * <li>Knowledge of value type at each point</li> When this is linearized, choices should be
    // * made to minimize gotos and conversions. Each conditional test can be change from IFNE to
    // IFEQ
    // * to get better results. If the current value is known to be true/false then a conditional
    // can
    // * be skipped, using flow of control instead of boolean comparisons. </br>
    // * [TODO] Alternatively, generate bad sequences and implement separate optimization.</br>
    // * The primary goal is to avoid complex type conversions everywhere. Another idea is to make
    // the
    // * return type more general, so it contains a map of data types and the labels used to return
    // * those types. The caller can put the labels anywhere that is useful to minimize conversions.
    // *
    // * @param il
    // * @param locals
    // * @param expr
    // * @param resultDesired
    // * @return
    // */
    // private Class<?> compile (final InsnList il, final Map<Symbol, LocalBinding> locals, final
    // Object expr,
    // final boolean resultDesired)
    // {
    // if (expr instanceof List)
    // {
    // return compileFunctionCall (il, locals, (LispList)expr, resultDesired);
    // }
    // else if (resultDesired)
    // {
    // if (expr instanceof Symbol)
    // {
    // return compileSymbolReference (il, locals, (Symbol)expr);
    // }
    // else
    // {
    // il.add (new LdcInsnNode (expr));
    // // Top result class is the same as the expr, perhaps converted to a primitive type.
    // final Class<?> ec = expr.getClass ();
    // final Class<?> p = boxer.getUnboxedClass (ec);
    // return p != null ? p : ec;
    // }
    // }
    // return null;
    // }
    //
    // private Class<?> compileFunctionCall (final InsnList il, final Map<Symbol, LocalBinding>
    // locals, final LispList expression,
    // final boolean resultDesired)
    // {
    // final Symbol symbol = expression.head ();
    // final FunctionCell function = symbol.getFunction ();
    // if (function != null)
    // {
    // if (function.getLispFunction () != null)
    // {
    // // Some 'normal' functions need special coding, i.e, arithmetic and comparisons, so
    // // this is called for any function with a compiler, not just special forms.
    // return compileSpecialLispFunction (il, locals, expression, resultDesired);
    // }
    // if (function instanceof SpecialFunctionCell)
    // {
    // // Since this is not a known special function, we are stuck and can't proceed
    // throw new IllegalArgumentException ("Unrecognized special form " + symbol);
    // }
    // if (function instanceof MacroFunctionCell)
    // {
    // // Expand and replace
    // final MacroFunctionCell macro = (MacroFunctionCell)function;
    // try
    // {
    // final Object replacement = macro.expand (expression);
    // return compile (il, locals, replacement, resultDesired);
    // }
    // catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e1)
    // {
    // throw new Error ("Error expanding macro " + symbol, e1);
    // }
    // }
    // if (optimizeFunctionCall (expression))
    // {
    // return compileDirectFunctionCall (il, expression, resultDesired);
    // }
    // }
    // return compileDefaultFunctionCall (il, locals, expression);
    // }
    //
    // private Class<?> compileSpecialLispFunction (final InsnList il, final Map<Symbol,
    // LocalBinding> locals,
    // final LispList expression, final boolean resultDesired)
    // {
    // final Symbol symbol = expression.head ();
    // final FunctionCell function = symbol.getFunction ();
    // if (function != null)
    // {
    // final LispFunction compiler = function.getLispFunction ();
    // if (compiler instanceof LispTreeFunction)
    // {
    // final LispTreeFunction c = (LispTreeFunction)compiler;
    // return c.compile (this, il, locals, expression, resultDesired);
    // }
    // }
    // return null;
    // }
    //
    // private boolean optimizeFunctionCall (final LispList expression)
    // {
    // return false;
    // }
    //
    // private Class<?> compileDirectFunctionCall (final InsnList il, final LispList expression,
    // final boolean resultDesired)
    // {
    // return null;
    // }
    //
    // /**
    // * Compile a function call and add it to the instruction list. This creates a generic function
    // * call to any normal function. <br/>
    // * [TODO] Special forms and macros need to be handled. <br/>
    // * [TODO] Optimized calls to known functions should also be produced.
    // *
    // * @param il The instruction list.
    // * @param locals Local variable binding information.
    // * @param expression The function call expression.
    // * @return The class of the return value.
    // */
    // private Class<?> compileDefaultFunctionCall (final InsnList il, final Map<Symbol,
    // LocalBinding> locals,
    // final LispList expression)
    // {
    // final Symbol symbol = expression.head ();
    // addSymbolReference (symbol);
    // LOGGER.finer (new LogString ("Function symbol reference to %s", symbol));
    // il.add (new VarInsnNode (ALOAD, 0));
    // final Type classType = compileLoader.getClassType ();
    // final String classInternalName = classType.getInternalName ();
    // il.add (new FieldInsnNode (GETFIELD, classInternalName, createJavaSymbolName (symbol),
    // "Llisp/Symbol;"));
    //
    // // Get the FunctionCell from the function symbol.
    // // The call to getDefaultHandlerFunction will return a DefaultHandler that tries to invoke
    // // the java method on arg 1 if the function has not been given any other definition.
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getDefaultHandlerFunction",
    // "()Llisp/symbol/FunctionCell;",
    // false));
    //
    // // Compile the arguments. Pass all the arguments as elements of a single array of Objects.
    // final int argCount = expression.size () - 1;
    // il.add (new LdcInsnNode (argCount));
    // il.add (new TypeInsnNode (ANEWARRAY, "java/lang/Object"));
    // for (int i = 0; i < argCount; i++)
    // {
    // il.add (new InsnNode (DUP));
    // il.add (new LdcInsnNode (i));
    // final Class<?> fromClass = compile (il, locals, expression.get (i + 1), true);
    // converter.convert (il, fromClass, Object.class, false, false);
    // il.add (new InsnNode (AASTORE));
    // }
    //
    // // Call invoke on the method retrieved from the FunctionCell of the function symbol.
    // // Assume the function will still be defined when we execute this code.
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply",
    // "([Ljava/lang/Object;)Ljava/lang/Object;",
    // false));
    // return Object.class;
    // }
    //
    // /**
    // * Compile an expression to calculate the value of a symbol. This determines if the symbol is
    // an
    // * argument, local variable or global symbol and calculates the correct value. <br/>
    // * [TODO] Constants and typed variables should be handled specially.
    // *
    // * @param mv The bytecode generator.
    // * @param symbol The symbol value to calculate.
    // * @return The class of the result produced.
    // */
    // private Class<?> compileSymbolReference (final InsnList il, final Map<Symbol, LocalBinding>
    // locals, final Symbol symbol)
    // {
    // if (methodArgs.contains (symbol))
    // {
    // // Parameter reference
    // final int argIndex = methodArgs.indexOf (symbol);
    // final Type argType = methodArgTypes.get (argIndex);
    // il.add (new VarInsnNode (argType.getOpcode (ILOAD), argIndex + 1));
    // final Class<?> fromClass = methodArgClasses.get (argIndex);
    // return fromClass;
    // }
    // else if (locals.containsKey (symbol))
    // {
    // // Reference to a local lexical variable
    // final LocalBinding lb = locals.get (symbol);
    // final int localRef = lb.getLocalRef ();
    // final Type varType = lb.getType ();
    // il.add (new VarInsnNode (varType.getOpcode (ILOAD), localRef));
    // final Class<?> fromClass = lb.getClass ();
    // return fromClass;
    // }
    // else if (symbol.is ("true"))
    // {
    // // Special case for global symbol "true"
    // il.add (new LdcInsnNode (true));
    // return boolean.class;
    // }
    // else if (symbol.is ("false"))
    // {
    // // Special case for global symbol "false"
    // il.add (new LdcInsnNode (false));
    // return boolean.class;
    // }
    // else
    // {
    // // Reference to a global variable
    // addGlobalReference (symbol); // Log message
    // addSymbolReference (symbol); // Make symbol available at execution time
    // // [TODO] If the symbol valueCell is constant, use the current value.
    // // [TODO] If the valueCell is a TypedValueCell, use the type information.
    // il.add (new VarInsnNode (ALOAD, 0));
    // final Type classType = compileLoader.getClassType ();
    // final String classInternalName = classType.getInternalName ();
    // il.add (new FieldInsnNode (GETFIELD, classInternalName, createJavaSymbolName (symbol),
    // "Llisp/Symbol;"));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;",
    // false));
    // return Object.class;
    // }
    // }

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

    public String createJavaSymbolName (final Symbol symbol)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("SYM_");
	final String symbolName = symbol.getName ();
	for (int i = 0; i < symbolName.length (); i++)
	{
	    final char c = symbolName.charAt (i);
	    if (Character.isJavaIdentifierPart (c))
	    {
		buffer.append (c);
	    }
	    else
	    {
		final int codePoint = symbolName.codePointAt (i);
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
	buffer.append (">");
	return buffer.toString ();
    }
}
