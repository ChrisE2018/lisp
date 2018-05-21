
package lisp.cc;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.Map.Entry;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.LocalVariablesSorter;

import lisp.*;
import lisp.Package;
import lisp.Symbol;
import lisp.symbol.*;

public class FunctionCompileClassAdaptor extends ClassVisitor implements Opcodes
{
    private final String className;
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

    // Compiler control.
    // On a simple example, using fields for symbol and function references makes the code 10%
    // faster.
    private final boolean useFieldForSymbolReferences = true;
    // private final boolean useFieldForFunctionReferences = true;

    public FunctionCompileClassAdaptor (final ClassVisitor cv, final String className, final String methodName,
            final LispList methodArgs, final LispList methodBody, final Map<String, Object> quotedReferencesMap)
    {
	super (Opcodes.ASM5, cv);
	this.className = className;
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
	System.out.printf ("Creating <init>(int) method for %s %n", className);
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, "<init>", "(I)V", null, null);
	mv.visitCode ();

	// Call super constructor.
	mv.visitVarInsn (ALOAD, 0);
	mv.visitMethodInsn (INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaName = createJavaSymbolName (symbol);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitLdcInsn (symbol.getPackage ().getName ());
	    mv.visitLdcInsn (symbol.getName ());
	    mv.visitMethodInsn (INVOKESPECIAL, className, "getPublicSymbol",
	            "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", false);
	    mv.visitFieldInsn (PUTFIELD, className, javaName, "Llisp/Symbol;");

	    System.out.printf ("Init: private Symbol %s %s; %n", javaName, symbol);
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
	    mv.visitTypeInsn (CHECKCAST, "lisp/cc/CompileLoader");
	    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/cc/CompileLoader", "getQuotedReferences", "()Ljava/util/Map;", false);

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

    // @SuppressWarnings ("unused")
    // private void createIGetter (final String mName, final String pName)
    // {
    // final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, mName, "()I", null, null);
    // mv.visitVarInsn (ALOAD, 0);
    // mv.visitFieldInsn (GETFIELD, className, pName, "I");
    // mv.visitInsn (IRETURN);
    // mv.visitMaxs (0, 0);
    // mv.visitEnd ();
    // }

    // @SuppressWarnings ("unused")
    // private void createGetter (final String mName, final String pName, final String returnType)
    // {
    // final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, mName, "()" + returnType, null, null);
    // mv.visitVarInsn (ALOAD, 0);
    // mv.visitFieldInsn (GETFIELD, className, pName, returnType);
    // mv.visitInsn (ARETURN);
    // mv.visitMaxs (0, 0);
    // mv.visitEnd ();
    // }

    private void compileDefinition ()
    {
	// Define method header
	final String returnType = "Ljava/lang/Object;";
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("(");
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    buffer.append ("Ljava/lang/Object;");
	}
	buffer.append (")");
	buffer.append (returnType);
	final String signature = buffer.toString ();
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, methodName, signature, null, null);
	// With LocalVariablesSorter we can allocate new local variables.
	// For example:
	// int time = newLocal(Type.LONG_TYPE);
	// creates a variable entry for a long that can be used like this:
	// mv.visitVarInsn(LSTORE, time);
	final LocalVariablesSorter mv2 = new LocalVariablesSorter (ACC_PUBLIC, signature, mv);
	// Compile method body
	for (final Object e : methodBody)
	{
	    compileExpression (mv2, e);
	}

	// Return and method coda
	mv.visitInsn (ARETURN);
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

    /** Compile a single expression and leave the value on top of the stack. */
    private void compileExpression (final MethodVisitor mv, final Object e)
    {
	if (e == null)
	{
	    System.out.printf ("Ignoring nested null %s%n", e);
	    mv.visitInsn (ACONST_NULL);
	}
	else if (e instanceof LispList)
	{
	    final LispList ee = (LispList)e;
	    if (ee.size () == 0)
	    {
		// Return e unchanged
		mv.visitInsn (ACONST_NULL);
	    }
	    else
	    {
		compileFunctionCall (mv, ee);
	    }
	}
	else if (e instanceof Symbol)
	{
	    if (methodArgs.contains (e))
	    {
		// Parameter reference
		// [TODO] If we can determine the type, use that information.
		final int argRef = methodArgs.indexOf (e) + 1;
		mv.visitVarInsn (ALOAD, argRef);
	    }
	    else if (localVariableMap.containsKey (e))
	    {
		final int localRef = localVariableMap.get (e);
		mv.visitVarInsn (ALOAD, localRef);
	    }
	    else
	    {
		final Symbol symbol = (Symbol)e;
		if (useFieldForSymbolReferences)
		{
		    if (!symbolReferences.contains (symbol))
		    {
			symbolReferences.add (symbol);
		    }
		    System.out.printf ("Symbol reference to %s %n", symbol);
		    // [TODO] If the symbol valueCell is constant, use the current value.
		    // [TODO] If the valueCell is a TypedValueCell, use the type information.
		    mv.visitVarInsn (ALOAD, 0);
		    mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");
		    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false);
		}
		else
		{
		    mv.visitVarInsn (ALOAD, 0);
		    mv.visitLdcInsn (symbol.getPackage ().getName ());
		    mv.visitLdcInsn (symbol.getName ());
		    mv.visitMethodInsn (INVOKESPECIAL, className, "getPublicSymbol",
		            "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", false);
		    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false);
		}

		if (!globalReferences.contains (symbol))
		{
		    globalReferences.add (symbol);
		    System.out.printf ("Compiled global reference to %s %n", symbol);
		}
	    }
	}
	// Compile constant expressions
	// [TODO] All of these box the constant in a class wrapper. If we can use the primitive type
	// instead, that is more efficient.
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
	    System.out.printf ("Ignoring '%s' %s%n", e, e.getClass ());
	    mv.visitInsn (ACONST_NULL);
	}
    }

    private void compileFunctionCall (final MethodVisitor mv, final LispList e)
    {
	// [TODO] Need to be able to compile a call to an undefined function (i.e. recursive call)
	System.out.printf ("Compile nested form %s%n", e);
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
	    compileSpecialFunctionCall (mv, f, e);
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
	    compileExpression (mv, replacement);
	}
	else
	{
	    compileStandardFunctionCall (mv, f, e);
	}
    }

    // (define foo () (not true))
    private void compileStandardFunctionCall (final MethodVisitor mv, final Symbol symbol, final LispList e)
    {
	// Save the symbol in a class field.
	// [TODO] If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.

	if (!symbolReferences.contains (symbol))
	{
	    symbolReferences.add (symbol);
	}
	System.out.printf ("Function symbol reference to %s %n", symbol);

	mv.visitVarInsn (ALOAD, 0);
	mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");

	// [TODO] The call to getFunction should be changed to call a smarter method that
	// can return a special FunctionCell which invokes the java method on arg 1 instead.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getDefaultHandlerFunction", "()Llisp/symbol/FunctionCell;", false);
	// Compile the arguments
	final int argCount = e.size () - 1;
	mv.visitInsn (ICONST_0 + argCount);
	mv.visitTypeInsn (ANEWARRAY, "java/lang/Object");
	for (int i = 0; i < argCount; i++)
	{
	    // [TODO] If we know argument types of the function we are about to call we can try to
	    // compile the expression more efficiently.
	    mv.visitInsn (DUP);
	    mv.visitInsn (ICONST_0 + i);
	    compileExpression (mv, e.get (i + 1));
	    mv.visitInsn (AASTORE);
	}
	// Call invoke on the method.
	// Assume the function will still be defined when we execute this code.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;", false);
    }

    private void compileSpecialFunctionCall (final MethodVisitor mv, final Symbol symbol, final LispList e)
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
	    compileQuote (mv, e);
	}
	else if (symbol.is ("progn"))
	{
	    compileProgn (mv, e);
	}
	else if (symbol.is ("if"))
	{
	    compileIf (mv, e);
	}
	else if (symbol.is ("and"))
	{
	    compileAnd (mv, e);
	}
	else if (symbol.is ("or"))
	{
	    compileOr (mv, e);
	}
	else if (symbol.is ("when"))
	{
	    compileWhen (mv, e);
	}
	else if (symbol.is ("unless"))
	{
	    compileUnless (mv, e);
	}
	else if (symbol.is ("setq"))
	{
	    compileSetq (mv, e);
	}
	else if (symbol.is ("repeat"))
	{
	    compileRepeat (mv, e);
	}
	else if (symbol.is ("while"))
	{
	    compileWhile (mv, e);
	}
	else if (symbol.is ("until"))
	{
	    compileUntil (mv, e);
	}
	else if (symbol.is ("let"))
	{
	    compileLet (mv, e);
	}
	else if (symbol.is ("let*"))
	{
	    compileLetStar (mv, e);
	}
	else if (symbol.is ("cond"))
	{
	    compileCond (mv, e);
	}
	else
	{
	    throw new IllegalArgumentException ("NYI special form " + symbol);
	}
    }

    private void compileQuote (final MethodVisitor mv, final LispList e)
    {
	// (define foo () (quote bar))
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
	// System.out.printf ("Quoted reference to %s (%s)%n", typeDescriptor, quoted);
	mv.visitVarInsn (ALOAD, 0);
	mv.visitFieldInsn (GETFIELD, className, reference.getName (), typeDescriptor);
    }

    private void compileProgn (final MethodVisitor mv, final LispList e)
    {
	// (define foo () (progn (printf "a%n") (printf "b%n") 3))
	if (e.size () == 0)
	{
	    mv.visitInsn (ACONST_NULL);
	}
	else
	{
	    for (int i = 1; i < e.size () - 1; i++)
	    {
		compileExpression (mv, e.get (i));
		mv.visitInsn (POP);
	    }
	    compileExpression (mv, e.last ());
	}
    }

    private void compileIf (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (if x 1 2))
	// (define foo (x) (if x 1 (printf "a%n") (printf "b%n") 3))
	mv.visitVarInsn (ALOAD, 0);
	compileExpression (mv, e.get (1));
	mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFEQ, l1);

	// True case
	compileExpression (mv, e.get (2));
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);

	mv.visitLabel (l1);
	// mv.visitFrame (Opcodes.F_SAME, 0, null, 0, null);
	if (e.size () <= 3)
	{
	    // No else case
	    mv.visitInsn (ACONST_NULL);
	}
	else
	{
	    // Else case
	    for (int i = 3; i < e.size () - 1; i++)
	    {
		compileExpression (mv, e.get (i));
		mv.visitInsn (POP);
	    }
	    // Don't pop the last value
	    compileExpression (mv, e.last ());
	}
	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l2);
	// mv.visitFrame (Opcodes.F_SAME1, 0, null, 1, new Object[]
	// {"java/lang/Object"});
    }

    private void compileAnd (final MethodVisitor mv, final LispList e)
    {
	// (define foo (a b) (and))
	// (define foo (a b) (and a b))
	if (e.size () <= 1)
	{
	    // True case
	    final Label l0 = new Label ();
	    mv.visitLabel (l0);
	    mv.visitLineNumber (99, l0);
	    mv.visitInsn (ICONST_1);
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	}
	else
	{
	    final Label l1 = new Label ();
	    final Label l2 = new Label ();
	    for (int i = 1; i < e.size (); i++)
	    {
		mv.visitVarInsn (ALOAD, 0);
		compileExpression (mv, e.get (i));
		mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn (IFEQ, l1);
	    }
	    // True case
	    mv.visitInsn (ICONST_1);
	    mv.visitJumpInsn (GOTO, l2);

	    // False case
	    mv.visitLabel (l1);
	    // mv.visitFrame (Opcodes.F_SAME, 0, null, 0, null);
	    mv.visitInsn (ICONST_0);

	    // Jump here after true case or fall through after false.
	    // Return final value.
	    mv.visitLabel (l2);
	    // mv.visitFrame (Opcodes.F_SAME1, 0, null, 1, new Object[]
	    // {INTEGER});
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	}
    }

    private void compileOr (final MethodVisitor mv, final LispList e)
    {
	// (define foo (a b) (or))
	// (define foo (a b) (or a b))
	if (e.size () <= 1)
	{
	    // True case
	    final Label l0 = new Label ();
	    mv.visitLabel (l0);
	    mv.visitLineNumber (99, l0);
	    mv.visitInsn (ICONST_0);
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	}
	else
	{
	    final Label l1 = new Label ();
	    final Label l2 = new Label ();
	    for (int i = 1; i < e.size (); i++)
	    {
		mv.visitVarInsn (ALOAD, 0);
		compileExpression (mv, e.get (i));
		mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn (IFNE, l1);
	    }
	    // False case
	    mv.visitInsn (ICONST_0);
	    mv.visitJumpInsn (GOTO, l2);

	    // True case
	    mv.visitLabel (l1);
	    // mv.visitFrame (Opcodes.F_SAME, 0, null, 0, null);
	    mv.visitInsn (ICONST_1);

	    // Jump here after false case or fall through after true.
	    // Return final value.
	    mv.visitLabel (l2);
	    // mv.visitFrame (Opcodes.F_SAME1, 0, null, 1, new Object[]
	    // {INTEGER});
	    mv.visitMethodInsn (INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
	}
    }

    private void compileWhen (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (when x 1 2))
	// (define foo (x) (when x 1 (printf "a%n") (printf "b%n") 3))
	mv.visitVarInsn (ALOAD, 0);
	compileExpression (mv, e.get (1));
	mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFEQ, l1);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i));
	    mv.visitInsn (POP);
	}
	// Don't pop the last value
	compileExpression (mv, e.last ());
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);

	mv.visitLabel (l1);
	// mv.visitFrame (Opcodes.F_SAME, 0, null, 0, null);
	mv.visitInsn (ACONST_NULL);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l2);
	// mv.visitFrame (Opcodes.F_SAME1, 0, null, 1, new Object[]
	// {"java/lang/Object"});
    }

    private void compileUnless (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (unless x 1 2))
	// (define foo (x) (unless x 1 (printf "a%n") (printf "b%n") 3))
	mv.visitVarInsn (ALOAD, 0);
	compileExpression (mv, e.get (1));
	mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);

	// True case
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i));
	    mv.visitInsn (POP);
	}
	// Don't pop the last value
	compileExpression (mv, e.last ());
	final Label l2 = new Label ();
	mv.visitJumpInsn (GOTO, l2);

	mv.visitLabel (l1);
	// mv.visitFrame (Opcodes.F_SAME, 0, null, 0, null);
	mv.visitInsn (ACONST_NULL);

	// Jump here after true case or fall through after else.
	// Return final value.
	mv.visitLabel (l2);
	// mv.visitFrame (Opcodes.F_SAME1, 0, null, 1, new Object[]
	// {"java/lang/Object"});
    }

    private void compileSetq (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (setq x 3))
	// (define foo (x) (setq x 3) x)
	// (define foo (x) (setq a x))
	final Symbol symbol = (Symbol)e.get (1);
	if (methodArgs.contains (symbol))
	{
	    // Parameter reference
	    // [TODO] If we can determine the type, use that information.
	    final int p = methodArgs.indexOf (symbol) + 1;
	    System.out.printf ("Setq parameter %s (%d) %n", symbol, p);
	    compileExpression (mv, e.get (2));
	    mv.visitInsn (DUP);
	    mv.visitVarInsn (ASTORE, p);
	}
	else
	{
	    if (useFieldForSymbolReferences)
	    {
		if (!symbolReferences.contains (symbol))
		{
		    symbolReferences.add (symbol);
		}
		System.out.printf ("Symbol assignment to %s %n", symbol);
		// [TODO] If the symbol valueCell is constant, use the current value.
		// [TODO] If the valueCell is a TypedValueCell, use the type information.
		mv.visitVarInsn (ALOAD, 0);
		mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");
	    }
	    else
	    {
		mv.visitVarInsn (ALOAD, 0);
		mv.visitLdcInsn (symbol.getPackage ().getName ());
		mv.visitLdcInsn (symbol.getName ());
		mv.visitMethodInsn (INVOKESPECIAL, className, "getPublicSymbol",
		        "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", false);
	    }
	    compileExpression (mv, e.get (2));
	    // Copy the expression value one back on the stack so it becomes the return value
	    mv.visitInsn (DUP_X1);
	    mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "setValue", "(Ljava/lang/Object;)V", false);
	    // Return the expression value
	    if (!globalReferences.contains (symbol))
	    {
		globalReferences.add (symbol);
		System.out.printf ("Compiled global assignment to %s %n", symbol);
	    }
	}
    }

    private void compileRepeat (final MethodVisitor mv, final LispList e)
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

	// Compute repeat count
	compileExpression (mv, e.get (1));
	mv.visitTypeInsn (CHECKCAST, "java/lang/Integer");
	mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false);
	// Leave repeat count on the stack

	// Push iteration number onto the stack
	mv.visitInsn (ICONST_0);

	// Jump to termination test
	final Label l1 = new Label ();
	mv.visitJumpInsn (GOTO, l1);

	// Start of iteration body
	final Label l2 = new Label ();
	mv.visitLabel (l2);
	// <body code goes here>
	for (int i = 2; i < e.size (); i++)
	{
	    compileExpression (mv, e.get (i));
	    mv.visitInsn (POP);
	}

	// Loop increment
	mv.visitInsn (ICONST_1);
	mv.visitInsn (IADD);

	// Termination test
	mv.visitLabel (l1);

	mv.visitInsn (DUP2);
	mv.visitInsn (SWAP);
	mv.visitJumpInsn (IF_ICMPLT, l2);

	// Remove iteration count
	mv.visitInsn (POP);
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
    }

    private void compileWhile (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (while (< a x) (printf "A: %s%n" a) (setq a (+ a 1))))

	// Load default value
	mv.visitInsn (ACONST_NULL);

	// Perform iteration test
	final Label l0 = new Label ();
	mv.visitLabel (l0);
	mv.visitVarInsn (ALOAD, 0);
	compileExpression (mv, e.get (1));
	mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFEQ, l1);

	// Get rid of previous value
	mv.visitInsn (POP);

	// Loop body
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i));
	    mv.visitInsn (POP);
	}
	// Don't pop the last value
	compileExpression (mv, e.last ());
	mv.visitJumpInsn (GOTO, l0);

	mv.visitLabel (l1);
    }

    private void compileUntil (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (setq a 0) (until (> a x) (printf "A: %s%n" a) (setq a (+ a 1))))

	// Load default value
	mv.visitInsn (ACONST_NULL);

	// Perform iteration test
	final Label l0 = new Label ();
	mv.visitLabel (l0);
	mv.visitVarInsn (ALOAD, 0);
	compileExpression (mv, e.get (1));
	mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	final Label l1 = new Label ();
	mv.visitJumpInsn (IFNE, l1);

	// Get rid of previous value
	mv.visitInsn (POP);

	// Loop body
	for (int i = 2; i < e.size () - 1; i++)
	{
	    compileExpression (mv, e.get (i));
	    mv.visitInsn (POP);
	}
	// Don't pop the last value
	compileExpression (mv, e.last ());
	mv.visitJumpInsn (GOTO, l0);

	mv.visitLabel (l1);
    }

    private void compileLet (final MethodVisitor mv, final LispList e)
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
	    compileExpression (mv, c.get (1));
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
	compileExpression (mv, e.get (2));
	// Evaluate remaining (optional) body forms
	for (int i = 3; i < e.size (); i++)
	{
	    // Get rid of previous value
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i));
	}
	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
    }

    private void compileLetStar (final MethodVisitor mv, final LispList e)
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
	    compileExpression (mv, c.get (1));
	    final int localRef = lvs.newLocal (Type.getType (Object.class));
	    mv.visitVarInsn (ASTORE, localRef);
	    localVariableMap.put (var, localRef);
	}

	// Evaluate first (required) body form
	compileExpression (mv, e.get (2));
	// Evaluate remaining (optional) body forms
	for (int i = 3; i < e.size (); i++)
	{
	    // Get rid of previous value
	    mv.visitInsn (POP);
	    compileExpression (mv, e.get (i));
	}
	// Restore original local variables map
	localVariableMap = savedLocalVariableMap;
    }

    private void compileCond (final MethodVisitor mv, final LispList e)
    {
	// (define foo (x) (cond ((= x 1) 'alpha) ((= x 2) 'beta) ((= x 3) 'gamma) (true 'delta)))
	final Package system = PackageFactory.getSystemPackage ();
	final Symbol var = system.internPrivate ("result").gensym ();
	final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;
	final int resultRef = lvs.newLocal (Type.getType (Object.class));
	localVariableMap.put (var, resultRef);
	mv.visitInsn (ACONST_NULL);
	mv.visitVarInsn (ASTORE, resultRef);
	// Label to goto and return result
	final Label l0 = new Label ();
	for (int i = 1; i < e.size (); i++)
	{
	    final LispList clause = (LispList)e.get (i);
	    final Object key = clause.get (0);
	    compileExpression (mv, key);
	    mv.visitInsn (DUP);
	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitInsn (SWAP);
	    mv.visitMethodInsn (INVOKESPECIAL, className, "isTrue", "(Ljava/lang/Object;)Z", false);
	    final Label l1 = new Label ();
	    mv.visitJumpInsn (IFEQ, l1);
	    mv.visitVarInsn (ASTORE, resultRef);
	    for (int j = 1; j < clause.size (); j++)
	    {
		compileExpression (mv, clause.get (j));
		mv.visitVarInsn (ASTORE, resultRef);
	    }
	    mv.visitJumpInsn (GOTO, l0);
	    mv.visitLabel (l1);
	    mv.visitInsn (POP);
	}
	// Return result
	mv.visitLabel (l0);
	mv.visitVarInsn (ALOAD, resultRef);
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
	    // System.out.printf ("Field: private Symbol %s; [%s]%n", name, symbol);
	}
	for (final Entry<Object, Symbol> entry : quotedReferences.entrySet ())
	{
	    final Object quoted = entry.getKey ();
	    final Symbol reference = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    // System.out.printf ("Field: private Quoted %s; [%s]%n", reference, quoted);
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
	buffer.append (">");
	return buffer.toString ();
    }
}
