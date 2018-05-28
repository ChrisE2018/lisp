
package lisp.cc;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.LocalVariablesSorter;

import lisp.*;
import lisp.Package;
import lisp.Symbol;
import lisp.symbol.*;

public class CompileClassAdaptor_v1 extends ClassVisitor implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (CompileLoader_v1.class.getName ());

    private final String className;
    private final Class<?> returnType;
    private final String methodName;
    private final LispList methodArgs;
    private final List<Object> methodBody;
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();
    private final Map<Object, Symbol> quotedReferences = new LinkedHashMap<Object, Symbol> ();

    /**
     * Inverse map of quoted references. This is created in the CompileLoader and saved here. When
     * it is modified, the class can get at it by getClassLoader().getQuotedReferences ();
     */
    private final Map<String, Object> quotedReferencesMap;

    private Map<Symbol, Integer> localVariableMap = new LinkedHashMap<Symbol, Integer> ();

    public CompileClassAdaptor_v1 (final ClassVisitor cv, final String className, final Class<?> returnType,
            final String methodName, final LispList methodArgs, final LispList methodBody,
            final Map<String, Object> quotedReferencesMap)
    {
	super (Opcodes.ASM5, cv);
	this.className = className;
	this.returnType = returnType;
	this.methodName = methodName;
	this.methodArgs = methodArgs;
	this.methodBody = methodBody;
	this.quotedReferencesMap = quotedReferencesMap;
    }

    /**
     * The int constructor is added to the CompiledShell class. Removing the no-arg constructor
     * proved to be difficult, but this is easy. This constructor should create fields for all
     * Symbol and structure references needed by the functions. Support classes might have to be
     * used as a repository to hold references to complex structures so they don't get copied.
     */
    private void createInitI ()
    {
	LOGGER.finer (String.format ("Creating <init>(int) method for %s", className));
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, "<init>", "(I)V", null, null);
	mv.visitCode ();

	final Type classLoaderType = Type.getType (CompileLoader_v1.class);
	final String classLoaderInternalName = classLoaderType.getInternalName ();
	// Call super constructor.
	mv.visitVarInsn (ALOAD, 0);
	mv.visitMethodInsn (INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);

	// Store the methodName in a field
	mv.visitVarInsn (ALOAD, 0);
	mv.visitLdcInsn (methodName);
	mv.visitFieldInsn (PUTFIELD, className, "methodName", "Ljava/lang/String;");

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaName = createJavaSymbolName (symbol);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitLdcInsn (symbol.getPackage ().getName ());
	    mv.visitLdcInsn (symbol.getName ());
	    mv.visitMethodInsn (INVOKESPECIAL, className, "getSymbol", "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;",
	            false);
	    mv.visitFieldInsn (PUTFIELD, className, javaName, "Llisp/Symbol;");

	    LOGGER.finer (String.format ("Init: private Symbol %s %s;", javaName, symbol));
	}
	for (final Entry<Object, Symbol> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    final Object quoted = entry.getKey ();
	    final Symbol reference = entry.getValue ();
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitInsn (DUP);
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Class", "getClassLoader", "()Ljava/lang/ClassLoader;", false);
	    mv.visitTypeInsn (CHECKCAST, classLoaderInternalName);
	    mv.visitMethodInsn (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;", false);

	    mv.visitLdcInsn (reference.getName ());
	    mv.visitMethodInsn (INVOKEINTERFACE, "java/util/Map", "get", "(Ljava/lang/Object;)Ljava/lang/Object;", true);
	    final Type quotedType = Type.getType (quoted.getClass ());
	    final String typeDescriptor = quotedType.getDescriptor ();
	    mv.visitTypeInsn (CHECKCAST, quotedType.getInternalName ());
	    mv.visitFieldInsn (PUTFIELD, className, reference.getName (), typeDescriptor);
	}
	mv.visitInsn (RETURN);
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
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

    // private void jumpIfFalse (final MethodVisitor mv, final Label l0)
    // {
    // // Assumes 'this' is on stack location before
    // mv.visitMethodInsn (INVOKESPECIAL, className, "isFalse", "(Ljava/lang/Object;)Z", false);
    // mv.visitJumpInsn (IFNE, l0);
    // }
    //
    // private void jumpIfNotFalse (final MethodVisitor mv, final Label l0)
    // {
    // // Assumes 'this' is on stack location before
    // mv.visitMethodInsn (INVOKESPECIAL, className, "isFalse", "(Ljava/lang/Object;)Z", false);
    // mv.visitJumpInsn (IFEQ, l0);
    // }
    //
    // private void jumpIfTrue (final MethodVisitor mv, final Label l0)
    // {// Assumes 'this' is on stack location before
    // mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
    // mv.visitJumpInsn (IFNE, l0);
    // }

    private void coerceBoolean (final MethodVisitor mv)
    {
	// (define boolean:foo () true)
	// (define boolean:foo (x) x)
	mv.visitInsn (DUP);
	final Label l1 = new Label ();
	mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	mv.visitJumpInsn (IFNE, l1);
	mv.visitInsn (POP);
	mv.visitLdcInsn (true);
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);
	mv.visitLabel (l1);
	mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	mv.visitLabel (l2);
    }

    private void compileDefinition ()
    {
	// Define method header
	final String returnTypeDescriptor = Type.getType (returnType).getDescriptor ();
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("(");
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    buffer.append (CompileSupport.getNameTypeDescriptor (methodArgs.get (i)));
	}
	buffer.append (")");
	buffer.append (returnTypeDescriptor);
	final String signature = buffer.toString ();
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, methodName, signature, null, null);
	// With LocalVariablesSorter we can allocate new local variables.
	// For example:
	// int time = newLocal(Type.LONG_TYPE);
	// creates a variable entry for a long that can be used like this:
	// mv.visitVarInsn(LSTORE, time);
	final LocalVariablesSorter mv2 = new LocalVariablesSorter (ACC_PUBLIC, signature, mv);
	// Compile method body
	final int bodyLimit = methodBody.size () - 1;
	for (int i = 0; i < bodyLimit; i++)
	{
	    final Object e = methodBody.get (i);
	    compileExpression (mv2, e, null);
	}
	final Object e = methodBody.get (bodyLimit);
	compileExpression (mv2, e, returnType);

	// Return and method coda
	// if (!returnType.equals (Object.class))
	// {
	// // [TODO] compileExpression should always do the right thing and this check should be
	// // eliminated
	// mv.visitTypeInsn (CHECKCAST, Type.getType (returnType).getInternalName ());
	// }

	mv.visitInsn (Type.getType (returnType).getOpcode (IRETURN));
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

    /** Compile a single expression and leave the value on top of the stack. */
    private void compileExpression (final MethodVisitor mv, final Object e, final Class<?> valueType)
    {
	// [TOOD] valueType is ignored for now except when it is null
	if (e == null)
	{
	    throw new Error ("Null is an illegal expression");
	}
	else if (e instanceof LispList)
	{
	    compileFunctionCall (mv, (LispList)e, valueType);
	}
	else if (e instanceof Symbol)
	{
	    final Symbol symbol = (Symbol)e;
	    if (methodArgs.contains (symbol))
	    {
		// Parameter reference
		// [TODO] If we can determine the type, use that information.
		if (valueType != null)
		{
		    final int argRef = methodArgs.indexOf (symbol) + 1;
		    mv.visitVarInsn (ALOAD, argRef);
		    if (valueType.equals (boolean.class))
		    {
			coerceBoolean (mv);
		    }
		}
	    }
	    else if (localVariableMap.containsKey (symbol))
	    {
		if (valueType != null)
		{
		    final int localRef = localVariableMap.get (symbol);
		    mv.visitVarInsn (ALOAD, localRef);
		    if (valueType.equals (boolean.class))
		    {
			coerceBoolean (mv);
		    }
		}
	    }
	    else if (symbol.is ("true") && valueType.equals (boolean.class))
	    {
		mv.visitLdcInsn (true);
	    }
	    else if (symbol.is ("false") && valueType.equals (boolean.class))
	    {
		mv.visitLdcInsn (false);
	    }
	    else
	    {
		if (valueType != null)
		{
		    if (!symbolReferences.contains (symbol))
		    {
			symbolReferences.add (symbol);
		    }
		    LOGGER.finer (String.format ("Symbol reference to %s", symbol));
		    // [TODO] If the symbol valueCell is constant, use the current value.
		    // [TODO] If the valueCell is a TypedValueCell, use the type information.

		    mv.visitVarInsn (ALOAD, 0);
		    mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");
		    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false);
		    if (valueType.equals (boolean.class))
		    {
			coerceBoolean (mv);
		    }
		    if (!globalReferences.contains (symbol))
		    {
			globalReferences.add (symbol);
			LOGGER.finer (String.format ("Compiled global reference to %s", symbol));
		    }
		}
	    }
	}
	else if (valueType != null)
	{
	    if (valueType.equals (boolean.class))
	    {
		if (e instanceof Boolean)
		{
		    mv.visitLdcInsn (e);
		}
		else
		{
		    mv.visitLdcInsn (true);
		}
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
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false);
	    }
	    else if (e instanceof Short)
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false);
	    }
	    else if (e instanceof Integer)
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	    }
	    else if (e instanceof Long)
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false);
	    }
	    else if (e instanceof Float)
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false);
	    }
	    else if (e instanceof Double)
	    {
		mv.visitLdcInsn (e);
		mv.visitMethodInsn (INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
	    }
	    else if (e instanceof String)
	    {
		mv.visitLdcInsn (e);
	    }
	    else
	    {
		LOGGER.info (String.format ("Ignoring '%s' %s", e, e.getClass ()));
		mv.visitInsn (ACONST_NULL);
	    }
	}
    }

    private void compileFunctionCall (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// Need to be able to compile a call to an undefined function (i.e. recursive call)
	LOGGER.finer (String.format ("Compile nested form %s", e));
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
	    compileSpecialFunctionCall (mv, f, e, valueType);
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
	    compileExpression (mv, replacement, valueType);
	}
	else
	{
	    compileStandardFunctionCall (mv, f, e, valueType);
	    if (boolean.class.equals (valueType))
	    {
		coerceBoolean (mv);
	    }
	}
    }

    // (define foo () (not true))
    private void compileStandardFunctionCall (final MethodVisitor mv, final Symbol symbol, final LispList e,
            final Class<?> valueType)
    {
	// Save the symbol in a class field.
	// [TODO] If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.

	if (!symbolReferences.contains (symbol))
	{
	    symbolReferences.add (symbol);
	}
	LOGGER.finer (String.format ("Function symbol reference to %s", symbol));

	mv.visitVarInsn (ALOAD, 0);
	mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");

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
	if (valueType == null)
	{
	    mv.visitInsn (POP);
	}
    }

    private void compileSpecialFunctionCall (final MethodVisitor mv, final Symbol symbol, final LispList e,
            final Class<?> valueType)
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
	// [TODO] Hookup definition of special function calls to DefineLisp annotation.
	// [TODO] Optimization
	if (symbol.is ("quote"))
	{
	    compileQuote (mv, e, valueType);
	}
	else if (symbol.is ("progn"))
	{
	    compileProgn (mv, e, valueType);
	}
	else if (symbol.is ("if"))
	{
	    if (e.size () <= 3)
	    { // No else clause, compile as when
		compileWhen (mv, e, valueType);
	    }
	    else
	    { // If then else
		compileIf (mv, e, valueType);
	    }
	}
	else if (symbol.is ("and"))
	{
	    compileAnd (mv, e, valueType);
	}
	else if (symbol.is ("or"))
	{
	    compileOr (mv, e, valueType);
	}
	else if (symbol.is ("when"))
	{
	    compileWhen (mv, e, valueType);
	}
	else if (symbol.is ("unless"))
	{
	    compileUnless (mv, e, valueType);
	}
	else if (symbol.is ("setq"))
	{
	    compileSetq (mv, e, valueType);
	}
	else if (symbol.is ("repeat"))
	{
	    compileRepeat (mv, e, valueType);
	}
	else if (symbol.is ("dotimes"))
	{
	    compileDotimes (mv, e, valueType);
	}
	else if (symbol.is ("while"))
	{
	    compileWhile (mv, e, valueType);
	}
	else if (symbol.is ("until"))
	{
	    compileUntil (mv, e, valueType);
	}
	else if (symbol.is ("let"))
	{
	    compileLet (mv, e, valueType);
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
	    compileLetStar (mv, e, valueType);
	}
	else if (symbol.is ("cond"))
	{
	    if (valueType == null)
	    {
		compileVoidCond (mv, e);
	    }
	    else if (valueType.equals (boolean.class))
	    {
		compileBooleanCond (mv, e);
	    }
	    else
	    {
		compileCond (mv, e, valueType);
	    }
	}
	else
	{
	    throw new IllegalArgumentException ("NYI special form " + symbol);
	}
    }

    private void compileQuote (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo () (quote bar))
	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    final Symbol quote = (Symbol)e.get (0);
	    final Symbol reference = quote.gensym ();
	    final Object quoted = e.get (1);
	    if (!quotedReferences.containsKey (quoted))
	    {
		// Save the reference and build it in the init method.
		quotedReferences.put (quoted, reference);
		quotedReferencesMap.put (reference.getName (), quoted);
	    }
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    // LOGGER.finer (String.format ("Quoted reference to %s (%s)", typeDescriptor, quoted));
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitFieldInsn (GETFIELD, className, reference.getName (), typeDescriptor);

	}
    }

    private void compileProgn (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo () (progn (printf "a%n") (printf "b%n") 3))
	if (e.size () == 0)
	{
	    if (boolean.class.equals (valueType))
	    {
		mv.visitLdcInsn (true);
	    }
	    else if (valueType != null)
	    {
		mv.visitInsn (ACONST_NULL);
	    }
	}
	else
	{
	    for (int i = 1; i < e.size () - 1; i++)
	    {
		compileExpression (mv, e.get (i), null);
	    }
	    compileExpression (mv, e.last (), valueType);
	}
    }

    private void compileIf (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (if x 1 2))
	// (define foo (x) (if x 1 (printf "a%n") (printf "b%n") 3))

	compileExpression (mv, e.get (1), boolean.class);
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	mv.visitJumpInsn (IFEQ, l2);

	// True case
	compileExpression (mv, e.get (2), valueType);
	mv.visitJumpInsn (GOTO, l1);

	// False case. Nothing on the stack.
	mv.visitLabel (l2);
	for (int i = 3; i < e.size () - 1; i++)
	{
	    // valueType null means nothing is left on the stack
	    compileExpression (mv, e.get (i), null);
	}
	// Leave the last value
	if (e.size () <= 3)
	{
	    if (valueType != null)
	    {
		mv.visitInsn (ACONST_NULL);
	    }
	}
	else
	{
	    compileExpression (mv, e.last (), valueType);
	}

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l1);
    }

    private void compileAnd (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (a b) (and))
	// (define foo (a b) (and a b))
	final Label l1 = new Label ();
	final Label l2 = new Label ();
	mv.visitInsn (ICONST_1);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	for (int i = 1; i < e.size (); i++)
	{
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i), Object.class /* TODO */);
	    mv.visitInsn (DUP);
	    final Label l3 = new Label ();
	    mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	    mv.visitJumpInsn (IFEQ, l3);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	    mv.visitJumpInsn (IFEQ, l1);
	    mv.visitLabel (l3);
	}
	// True case
	mv.visitJumpInsn (GOTO, l2);

	// False case
	mv.visitLabel (l1);
	mv.visitInsn (POP);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);

	// Jump here after true case or fall through after false.
	// Return final value.
	mv.visitLabel (l2);
	if (valueType == null)
	{
	    mv.visitInsn (POP);
	}
	if (boolean.class.equals (valueType))
	{
	    coerceBoolean (mv);
	}
    }

    private void compileOr (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (a b) (or))
	// (foo 1 2)
	// (define foo (a b) (or a b))
	final Label l1 = new Label ();
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	for (int i = 1; i < e.size (); i++)
	{
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i), Object.class /* TODO */);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
	    mv.visitJumpInsn (IFEQ, l1);
	    mv.visitInsn (DUP);
	    mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
	    mv.visitJumpInsn (IFNE, l1);
	}
	// False case
	mv.visitInsn (POP);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);

	// Jump here for true case or fall through in false case
	mv.visitLabel (l1);

	if (valueType == null)
	{
	    mv.visitInsn (POP);
	}
	if (boolean.class.equals (valueType))
	{
	    coerceBoolean (mv);
	}
    }

    private void compileWhen (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (when x 1 2))
	// (define foo (x) (when x 1 (printf "a%n") (printf "b%n") 3))

	final Label l1 = new Label ();
	final Label l2 = new Label ();
	compileExpression (mv, e.get (1), boolean.class);
	mv.visitJumpInsn (IFEQ, l2);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Don't pop the last value
	compileExpression (mv, e.last (), valueType);
	mv.visitJumpInsn (GOTO, l1);

	// False case.
	mv.visitLabel (l2);

	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    // False case where value is required
	    mv.visitInsn (ACONST_NULL);
	}

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l1);
    }

    private void compileUnless (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (unless x 1 2))
	// (define foo (x) (unless x 1 (printf "a%n") (printf "b%n") 3))
	compileExpression (mv, e.get (1), boolean.class);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Don't pop the last value
	compileExpression (mv, e.last (), valueType);
	final Label l3 = new Label ();
	mv.visitJumpInsn (GOTO, l3);

	// False case where we must pop a value
	mv.visitLabel (l1);

	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    mv.visitInsn (ACONST_NULL);
	}

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l3);
    }

    private void compileSetq (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (setq x 3))
	// (define foo (x) (setq x 3) x)
	// (define foo (x) (setq a x))
	// (define foo (x) (let ((a 3)) (setq a (+ a x)) a))
	final Symbol symbol = (Symbol)e.get (1);
	if (methodArgs.contains (symbol))
	{
	    // Parameter reference
	    // [TODO] If we can determine the type, use that information.
	    final int argRef = methodArgs.indexOf (symbol) + 1;
	    LOGGER.finer (String.format ("Setq parameter %s (%d)", symbol, argRef));
	    compileLocalSetq (mv, argRef, e.get (2), valueType);
	}
	else if (localVariableMap.containsKey (symbol))
	{
	    final int localRef = localVariableMap.get (symbol);
	    LOGGER.finer (String.format ("Setq local %s (%d)", symbol, localRef));
	    compileLocalSetq (mv, localRef, e.get (2), valueType);
	}
	else
	{
	    if (!symbolReferences.contains (symbol))
	    {
		symbolReferences.add (symbol);
	    }
	    LOGGER.finer (String.format ("Symbol assignment to %s", symbol));
	    // [TODO] If the symbol valueCell is constant, use the current value.
	    // [TODO] If the valueCell is a TypedValueCell, use the type information.
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");
	    compileExpression (mv, e.get (2), Object.class /* TODO */);
	    if (valueType != null)
	    {
		// Copy the expression value so it becomes the return value
		mv.visitInsn (DUP_X1);
	    }

	    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V", false);
	    // Return the expression value
	    if (!globalReferences.contains (symbol))
	    {
		globalReferences.add (symbol);
		LOGGER.finer (String.format ("Compiled global assignment to %s", symbol));
	    }
	    if (boolean.class.equals (valueType))
	    {
		coerceBoolean (mv);
	    }
	}
    }

    private void compileLocalSetq (final MethodVisitor mv, final int localRef, final Object expr, final Class<?> valueType)
    {
	if (valueType == null)
	{
	    compileExpression (mv, expr, Object.class /* TODO */);
	    mv.visitVarInsn (ASTORE, localRef);
	}
	else if (boolean.class.equals (valueType))
	{
	    compileExpression (mv, expr, Object.class /* TODO */);
	    mv.visitInsn (DUP);
	    mv.visitVarInsn (ASTORE, localRef);
	    coerceBoolean (mv);
	}
	else
	{
	    compileExpression (mv, expr, Object.class /* TODO */);
	    mv.visitInsn (DUP);
	    mv.visitVarInsn (ASTORE, localRef);
	}
    }

    private void compileRepeat (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (repeat x 3))
	// (define foo (x) (printf "bar%n"))
	// (define foo (x) (repeat x (printf "bar%n")))
	// (define foo (x) (repeat x (not true)))
	// (define foo (x) (repeat x true))
	// (define foo (x) (repeat x (abs 5)))
	// (define foo (x) (repeat x (printf "bar %s%n" x)))
	// (define foo (x) (repeat x (setq a (+ a 1))))
	// (define foo (x) (repeat (+ x 5) (setq a (+ a 1))))
	// (define foo (x y) (repeat (+ x 5) (setq a (+ a y))))

	// Make a local variable for repeat count
	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;
	final int countRef = lvs.newLocal (Type.getType (int.class));

	// Compute repeat count
	compileExpression (mv, e.get (1), Object.class /* TODO */);
	mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	// Put repeat count number into local variable
	mv.visitVarInsn (ISTORE, countRef);

	// Push default return value onto the stack
	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    mv.visitInsn (ACONST_NULL);
	}

	// Push iteration number onto the stack
	mv.visitInsn (ICONST_0);

	// Jump to termination test
	final Label l1 = new Label ();
	mv.visitJumpInsn (GOTO, l1);

	// // Start of iteration body
	final Label l2 = new Label ();
	mv.visitLabel (l2);
	// Stack: iteration, value

	// <body code goes here>
	if (valueType != null)
	{
	    mv.visitInsn (SWAP);
	}
	// Stack: value, iteration
	for (int i = 2; i < e.size (); i++)
	{
	    if (valueType != null)
	    {
		mv.visitInsn (POP);
	    }
	    compileExpression (mv, e.get (i), valueType);
	}
	if (valueType != null)
	{
	    mv.visitInsn (SWAP);
	}

	// Loop increment
	// Stack: iteration, value
	mv.visitInsn (ICONST_1);
	mv.visitInsn (IADD);

	// Termination test
	// Stack: iteration, value
	mv.visitLabel (l1);

	mv.visitInsn (DUP);
	mv.visitVarInsn (ILOAD, countRef);
	// Stack: count, iteration, iteration, value
	mv.visitJumpInsn (IF_ICMPLT, l2);
	// Stack: iteration, value
	// Remove iteration count
	mv.visitInsn (POP);
    }

    private void compileDotimes (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo () (repeat 3 0))
	// (define foo () (dotimes (i 3) 0))
	// (define foo (n) (dotimes (i n) 0))
	// (define foo () (dotimes (i 3) (printf "i = %s %n" i)))
	// (define foo (n) (dotimes (i n) (printf "i = %s %n" i)))

	// Compute repeat count
	final List<?> control = (List<?>)e.get (1);
	final Object count = control.get (1);
	compileExpression (mv, count, Object.class /* TODO */);
	mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	// Leave repeat count on the stack

	// Push default return value onto the stack
	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    mv.visitInsn (ACONST_NULL);
	}

	// Put iteration number into local variable
	final Map<Symbol, Integer> savedLocalVariableMap = localVariableMap;
	localVariableMap = new LinkedHashMap<Symbol, Integer> (localVariableMap);
	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;
	final int iterationRef = lvs.newLocal (Type.getType (Integer.class));
	final Symbol var = (Symbol)control.get (0);
	localVariableMap.put (var, iterationRef);
	mv.visitInsn (ICONST_0);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	mv.visitVarInsn (ASTORE, iterationRef);

	// Jump to termination test
	final Label l1 = new Label ();
	mv.visitJumpInsn (GOTO, l1);

	// Start of iteration body
	final Label l2 = new Label ();
	mv.visitLabel (l2);
	if (valueType != null)
	{
	    mv.visitInsn (SWAP); // Save repeat count
	}
	// <body code goes here>
	for (int i = 2; i < e.size (); i++)
	{
	    if (valueType != null)
	    {
		mv.visitInsn (POP);
	    }
	    compileExpression (mv, e.get (i), valueType);
	}

	// Loop increment
	mv.visitVarInsn (ALOAD, iterationRef);
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	mv.visitInsn (ICONST_1);
	mv.visitInsn (IADD);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
	mv.visitVarInsn (ASTORE, iterationRef);

	// // Termination test
	mv.visitLabel (l1);
	if (valueType != null)
	{
	    mv.visitInsn (SWAP);
	}
	mv.visitInsn (DUP); // Dup count
	mv.visitVarInsn (ALOAD, iterationRef);
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	mv.visitJumpInsn (IF_ICMPGT, l2);

	mv.visitInsn (POP); // Remove repeat count
	// Return last body value
	// if (valueType == null)
	// {
	// mv.visitInsn (POP);
	// }
	// if (boolean.class.equals (valueType))
	// {
	// coerceBoolean (mv);
	// }
	localVariableMap = savedLocalVariableMap;
    }

    private void compileWhile (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (setq a 0) (while (< a x) (printf "A: %s%n" a) (setq a (+ a 1))))

	// Load default value
	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    mv.visitInsn (ACONST_NULL);
	}

	// Perform iteration test
	final Label l1 = new Label ();
	mv.visitLabel (l1);
	final Label l2 = new Label ();
	compileExpression (mv, e.get (1), boolean.class);
	mv.visitJumpInsn (IFEQ, l2);

	// Loop body
	if (valueType != null)
	{
	    mv.visitInsn (POP);
	}
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Don't pop the last value
	compileExpression (mv, e.last (), valueType);
	mv.visitJumpInsn (GOTO, l1);

	mv.visitLabel (l2);
	// if (boolean.class.equals (valueType))
	// {
	// coerceBoolean (mv);
	// }
    }

    private void compileUntil (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (setq a 0) (until (> a x) (printf "A: %s%n" a) (setq a (+ a 1))))

	// Load default value
	if (boolean.class.equals (valueType))
	{
	    mv.visitLdcInsn (true);
	}
	else if (valueType != null)
	{
	    mv.visitInsn (ACONST_NULL);
	}

	// Perform iteration test
	final Label l1 = new Label ();
	mv.visitLabel (l1);
	final Label l2 = new Label ();
	compileExpression (mv, e.get (1), boolean.class);
	mv.visitJumpInsn (IFNE, l2);

	// Loop body
	if (valueType != null)
	{
	    mv.visitInsn (POP);
	}
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i), null);
	}
	// Don't pop the last value
	compileExpression (mv, e.last (), valueType);
	mv.visitJumpInsn (GOTO, l1);

	mv.visitLabel (l2);
	// if (boolean.class.equals (valueType))
	// {
	// coerceBoolean (mv);
	// }
    }

    private void compileLet (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (let ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let ((a 1) (b 2)) a))
	// (define foo () (let ((a b) (b a)) a))
	// (define foo (x) (let ((a b) (b a)) (if x a b)))
	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;

	// Compile expression values onto the stack in order
	final LispList args = (LispList)e.get (1);
	for (final Object clause : args)
	{
	    final LispList c = (LispList)clause;
	    compileExpression (mv, c.get (1), Object.class /* TODO */);
	}

	// Now bind the variables in reverse order
	final Map<Symbol, Integer> savedLocalVariableMap = localVariableMap;
	localVariableMap = new LinkedHashMap<Symbol, Integer> (localVariableMap);
	for (int i = args.size () - 1; i >= 0; i--)
	{
	    final Object clause = args.get (i);
	    final LispList c = (LispList)clause;
	    final Symbol var = (Symbol)c.get (0);
	    final int localRef = lvs.newLocal (Type.getType (Object.class));
	    mv.visitVarInsn (ASTORE, localRef); // Reverse order
	    localVariableMap.put (var, localRef);
	}

	// Evaluate first (required) body form
	compileExpression (mv, e.get (2), Object.class /* TODO */);
	// Evaluate remaining (optional) body forms
	for (int i = 3; i < e.size (); i++)
	{
	    // Get rid of previous value
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i), Object.class /* TODO */);
	}
	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
	if (valueType == null)
	{
	    mv.visitInsn (POP);
	}
	if (boolean.class.equals (valueType))
	{
	    coerceBoolean (mv);
	}
    }

    private void compileLetStar (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (let* ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let* ((a 1) (b 2)) a))
	// (define foo () (let* ((a b) (b a)) b))
	// (define bar () (let ((a b) (b a)) b))
	// (define foo (x) (let* ((a b) (b a)) (if x a b)))
	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;

	// Compile expression values onto the stack in order
	// Bind the variables as each value is computed
	final Map<Symbol, Integer> savedLocalVariableMap = localVariableMap;
	localVariableMap = new LinkedHashMap<Symbol, Integer> (localVariableMap);
	final LispList args = (LispList)e.get (1);
	for (final Object clause : args)
	{
	    final LispList c = (LispList)clause;
	    final Symbol var = (Symbol)c.get (0);
	    compileExpression (mv, c.get (1), Object.class /* TODO */);
	    final int localRef = lvs.newLocal (Type.getType (Object.class));
	    mv.visitVarInsn (ASTORE, localRef);
	    localVariableMap.put (var, localRef);
	}

	// Evaluate first (required) body form
	compileExpression (mv, e.get (2), Object.class /* TODO */);
	// Evaluate remaining (optional) body forms
	for (int i = 3; i < e.size (); i++)
	{
	    // Get rid of previous value
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i), Object.class /* TODO */);
	}
	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
	if (valueType == null)
	{
	    mv.visitInsn (POP);
	}
	if (boolean.class.equals (valueType))
	{
	    coerceBoolean (mv);
	}
    }

    private void compileCond (final MethodVisitor mv, final LispList e, final Class<?> valueType)
    {
	// (define foo (x) (cond ((= x 1) 'alpha) ((= x 2) 'beta) ((= x 3) 'gamma) (true 'delta)))
	final Package system = PackageFactory.getSystemPackage ();
	final Symbol var = system.internSymbol ("result").gensym ();

	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;
	int resultRef = 0;
	if (valueType != null)
	{
	    resultRef = lvs.newLocal (Type.getType (Object.class));
	    localVariableMap.put (var, resultRef);
	    mv.visitInsn (ACONST_NULL);
	    mv.visitVarInsn (ASTORE, resultRef);
	}
	// Label to goto and return result
	final Label l1 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final Object key = clause.get (0);
	    final Label l2 = new Label ();
	    final Label l3 = new Label ();
	    if (valueType == null)
	    {
		compileExpression (mv, key, boolean.class);
		mv.visitJumpInsn (IFEQ, l3);
	    }
	    else
	    {
		compileExpression (mv, key, valueType);
		mv.visitInsn (DUP);
		mv.visitTypeInsn (INSTANCEOF, "java/lang/Boolean");
		mv.visitJumpInsn (IFEQ, l2); // Check for boolean

		mv.visitInsn (DUP);
		mv.visitTypeInsn (CHECKCAST, "java/lang/Boolean");
		mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false);
		mv.visitJumpInsn (IFEQ, l3);
	    }

	    // Clause selected
	    mv.visitLabel (l2);
	    if (valueType != null)
	    {
		mv.visitVarInsn (ASTORE, resultRef);
	    }
	    for (int j = 1; j < clause.size (); j++)
	    {
		compileExpression (mv, clause.get (j), valueType);
	    }
	    if (valueType != null)
	    {
		mv.visitVarInsn (ASTORE, resultRef);
	    }
	    mv.visitJumpInsn (GOTO, l1);

	    // Clause not selected
	    mv.visitLabel (l3);
	    mv.visitInsn (POP);
	}
	// Return result
	mv.visitLabel (l1);
	if (valueType != null)
	{
	    mv.visitVarInsn (ALOAD, resultRef);
	}
    }

    /** Case where no return value is required. */
    private void compileVoidCond (final MethodVisitor mv, final LispList e)
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
    private void compileBooleanCond (final MethodVisitor mv, final LispList e)
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
	    // LOGGER.finer (String.format ("Field: private Symbol %s; [%s]", name, symbol));
	}
	for (final Entry<Object, Symbol> entry : quotedReferences.entrySet ())
	{
	    final Object quoted = entry.getKey ();
	    final Symbol reference = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    // LOGGER.finer (String.format ("Field: private Quoted %s; [%s]", reference, quoted));
	    createField (ACC_PRIVATE, reference.getName (), typeDescriptor);
	}
	createInitI ();
	cv.visitEnd ();
    }

    /**
     * Turn a symbol name into something acceptable to Java. Lisp symbols can include characters
     * like '+' that are not allowed in Java.
     */
    private String createJavaSymbolName (final Symbol symbol)
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
