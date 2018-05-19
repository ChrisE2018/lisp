
package lisp.cc;

import java.util.*;

import org.objectweb.asm.*;

import lisp.LispList;
import lisp.Symbol;
import lisp.eval.*;

public class FunctionCompileClassAdaptor extends ClassVisitor implements Opcodes
{
    private final String className;
    private final String methodName;
    private final LispList methodArgs;
    private final List<Object> methodBody;
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

    private final boolean useFieldForSymbolReferences = true;
    private final boolean useFieldForFunctionReferences = true;

    public FunctionCompileClassAdaptor (final ClassVisitor cv, final String className, final String methodName,
            final LispList methodArgs, final List<Object> methodBody)
    {
	super (Opcodes.ASM5, cv);
	this.className = className;
	this.methodName = methodName;
	this.methodArgs = methodArgs;
	this.methodBody = methodBody;
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

	// C reate initialization code for all entries in symbolReferences.
	// new[
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
	// ]new
	mv.visitInsn (RETURN);
	// Define local variables here.
	// final Label l3 = new Label ();
	// mv.visitLabel (l3);
	// mv.visitLocalVariable ("this", className, null, l0, l3, 0);
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

    @SuppressWarnings ("unused")
    private void createIGetter (final String mName, final String pName)
    {
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, mName, "()I", null, null);
	mv.visitVarInsn (ALOAD, 0);
	mv.visitFieldInsn (GETFIELD, className, pName, "I");
	mv.visitInsn (IRETURN);
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

    @SuppressWarnings ("unused")
    private void createGetter (final String mName, final String pName, final String returnType)
    {
	final MethodVisitor mv = cv.visitMethod (ACC_PUBLIC, mName, "()" + returnType, null, null);
	mv.visitVarInsn (ALOAD, 0);
	mv.visitFieldInsn (GETFIELD, className, pName, returnType);
	mv.visitInsn (ARETURN);
	mv.visitMaxs (0, 0);
	mv.visitEnd ();
    }

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

	// Compile method body
	for (final Object e : methodBody)
	{
	    compileExpression (mv, e);
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
	else if (e instanceof List<?>)
	{
	    final List<?> ee = (List<?>)e;
	    if (ee.size () == 0)
	    {
		// Return e unchanged
		mv.visitInsn (ACONST_NULL);
	    }
	    else
	    {
		compileFunctionCall (mv, (List<?>)e);
	    }
	}
	else if (e instanceof Symbol)
	{
	    if (methodArgs.contains (e))
	    {
		// Parameter reference
		// [TODO] If we can determine the type, use that information.
		final int p = methodArgs.indexOf (e) + 1;
		mv.visitVarInsn (ALOAD, p);
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

    private void compileFunctionCall (final MethodVisitor mv, final List<?> e)
    {
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
	if (function != null)
	{
	    if (function instanceof MacroFunctionCell)
	    {
		// Expand and replace
		throw new IllegalArgumentException ("NYI macro call " + f);
	    }
	    else if (function instanceof StandardFunctionCell)
	    {
		compileStandardFunctionCall (mv, f, e);
	    }
	    else if (function instanceof SpecialFunctionCell)
	    {
		// Unless this is a known special function, we are stuck and can't
		// proceed
		throw new IllegalArgumentException ("NYI special form " + f);
	    }
	}
	else if (e.size () > 1)
	{
	    // Handle unbound functions as calls to native Java methods
	    throw new IllegalArgumentException ("NYI java method call " + f);
	}
	else
	{
	    // Instead of throwing a compile time exception, this could produce runtime
	    // code to extract the symbol function.
	    throw new IllegalArgumentException ("Undefined function " + f);
	}
    }

    // (define foo () (not true))
    private void compileStandardFunctionCall (final MethodVisitor mv, final Symbol symbol, final List<?> e)
    {
	// Save the symbol in a class field.
	// [TODO] If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.
	if (useFieldForFunctionReferences)
	{
	    if (!symbolReferences.contains (symbol))
	    {
		symbolReferences.add (symbol);
	    }
	    System.out.printf ("Function symbol reference to %s %n", symbol);

	    mv.visitVarInsn (ALOAD, 0);
	    mv.visitFieldInsn (GETFIELD, className, createJavaSymbolName (symbol), "Llisp/Symbol;");
	}
	else
	{
	    final String name = symbol.getName ();
	    mv.visitVarInsn (ALOAD, 0);
	    // Get the package final from the original symbol package.
	    mv.visitLdcInsn (symbol.getPackage ().getName ());
	    mv.visitLdcInsn (name);
	    mv.visitMethodInsn (INVOKESPECIAL, className, "getPublicSymbol",
	            "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", false);
	}
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/Symbol", "getFunction", "()Llisp/eval/FunctionCell;", false);
	mv.visitTypeInsn (CHECKCAST, "lisp/eval/StandardFunctionCell");
	// Compile the arguments
	final int argCount = e.size () - 1;
	mv.visitInsn (ICONST_0 + argCount);
	mv.visitTypeInsn (ANEWARRAY, "java/lang/Object");
	for (int i = 0; i < argCount; i++)
	{
	    mv.visitInsn (DUP);
	    mv.visitInsn (ICONST_0 + i);
	    compileExpression (mv, e.get (i + 1));
	    mv.visitInsn (AASTORE);
	}
	// Call invoke on the method.
	// Assume the function will have the same definition when we execute this code.
	mv.visitMethodInsn (INVOKEVIRTUAL, "lisp/eval/StandardFunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;",
	        false);
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
	    createField (ACC_PRIVATE, name, "Llisp/Symbol;");
	    System.out.printf ("Field: private Symbol %s; [%s]%n", name, symbol);
	}
	createInitI ();
	// createIGetter ("getX", "X");
	// createGetter ("getNewField", NEW_FIELD_NAME, "Ljava/lang/String;");
	// createGetter ("getFooField", "foo", "Ljava/lang/Object;");
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
