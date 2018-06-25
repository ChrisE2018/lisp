
package lisp.cc;

import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.lang.PackageFactory;
import lisp.lang.Symbol;
import lisp.util.*;

/**
 * One way to preserve quoted data is to store it in the class loader. This won't work if the
 * bytecode is saved in a file. It only works if the bytecode is immediately loaded into the same
 * JVM by the same class loader.
 */
public class LispQuotedClassLoader extends ClassLoader implements QuotedData
{
    private static final Logger LOGGER = Logger.getLogger (LispQuotedClassLoader.class.getName ());
    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private static JavaName javaName = new JavaName ();
    private static final Type OBJECT_TYPE = Type.getType (Object.class);

    /** Fields containing quoted data required by the compilation class. */
    private final Map<Symbol, Object> quotedReferences = new LinkedHashMap<Symbol, Object> ();

    /** References to symbols that must be available to the bytecode. */
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

    protected Class<?> defineClass (final String name, final byte[] b) throws ClassFormatError
    {
	return super.defineClass (name, b, 0, b.length);
    }

    /**
     * Get the name from created names to quoted objects. This only works because compiled code is
     * being loaded into the same environment where it is compiled. To save compiled code to a file
     * would required building the structure in the init method when the class is loaded.
     */
    public Map<String, Object> getQuotedReferences ()
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    result.put (entry.getKey ().getName (), entry.getValue ());
	}
	return result;
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

    /** Get the saved quoted data. */
    @Override
    public Map<Symbol, Object> getQuotedData ()
    {
	return quotedReferences;
    }

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param quoted The quoted data to be stored.
     * @return The symbol that will name the data field. This is a generated unique symbol.
     */
    @Override
    public Symbol addQuotedConstant (final Object quoted)
    {
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    final Object q = entry.getValue ();
	    if (quoted.equals (q))
	    {
		return entry.getKey ();
	    }
	}
	final Symbol reference = QUOTE_SYMBOL.gensym ();
	quotedReferences.put (reference, quoted);
	return reference;
    }

    @Override
    public void addRequiredFields (final ClassNode cn)
    {
	final int hiddenFieldAccess = Opcodes.ACC_PRIVATE;
	// Create field definitions for all entries in symbolReferences.
	if (symbolReferences.size () > 0)
	{
	    for (final Symbol symbol : symbolReferences)
	    {
		final String typeDescriptor = Type.getType (symbol.getClass ()).getDescriptor ();
		final String javaSymbolName = javaName.createJavaSymbolName (symbol);
		cn.fields.add (new FieldNode (hiddenFieldAccess, javaSymbolName, typeDescriptor, null, null));
	    }
	}

	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    final Symbol reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    cn.fields.add (new FieldNode (hiddenFieldAccess, reference.getName (), typeDescriptor, null, null));
	}
	if (!symbolReferences.isEmpty () || !quotedReferences.isEmpty ())
	{
	    LOGGER.fine ("Adding GetSymbol method to " + cn.name);
	    final MethodNode symbolMethod = getGetSymbolMethod ();
	    cn.methods.add (symbolMethod);
	}
    }

    /** Create a method to locate a Symbol at runtime. */
    private MethodNode getGetSymbolMethod ()
    {
	final MethodNode mn = new MethodNode (Opcodes.ACC_PRIVATE, "getSymbol",
	        "(Ljava/lang/String;Ljava/lang/String;)Llisp/lang/Symbol;", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (Opcodes.ALOAD, 1));
	il.add (new MethodInsnNode (Opcodes.INVOKESTATIC, "lisp/lang/PackageFactory", "getPackage",
	        "(Ljava/lang/String;)Llisp/lang/Package;", false));
	il.add (new VarInsnNode (Opcodes.ALOAD, 2));
	il.add (new MethodInsnNode (Opcodes.INVOKEVIRTUAL, "lisp/lang/Package", "findSymbol",
	        "(Ljava/lang/String;)Llisp/lang/Symbol;", false));
	il.add (new InsnNode (Opcodes.ARETURN));

	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    @Override
    public void addHiddenConstructorSteps (final Type classType, final MethodVisitor mv)
    {
	final String classInternalName = classType.getInternalName ();
	if (!symbolReferences.isEmpty ())
	{
	    final Type stringType = Type.getType (String.class);
	    final Type symbolType = Type.getType (Symbol.class);
	    final String symbolTypeDescriptor = symbolType.getDescriptor ();
	    // Create initialization code for all required symbols.
	    for (final Symbol symbol : symbolReferences)
	    {
		final String javaSymbolName = javaName.createJavaSymbolName (symbol);
		mv.visitVarInsn (Opcodes.ALOAD, 0);
		mv.visitVarInsn (Opcodes.ALOAD, 0);
		mv.visitLdcInsn (symbol.getPackage ().getName ());
		mv.visitLdcInsn (symbol.getName ());
		mv.visitMethodInsn (Opcodes.INVOKESPECIAL, classInternalName, "getSymbol",
		        Type.getMethodDescriptor (symbolType, stringType, stringType), false);
		mv.visitFieldInsn (Opcodes.PUTFIELD, classInternalName, javaSymbolName, symbolTypeDescriptor);

		LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaSymbolName, symbol));
	    }
	}
	// final Map<Symbol, Object> quotedReferences = quotedData.getQuotedData ();
	if (!quotedReferences.isEmpty ())
	{
	    // Create initialization code for all required quoted data.
	    for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	    {
		// (define foo () (quote bar))
		final Symbol reference = entry.getKey ();
		final Object quoted = entry.getValue ();
		loadQuotedData (mv, reference);

		final Type quotedType = Type.getType (quoted.getClass ());
		final String typeDescriptor = quotedType.getDescriptor ();
		mv.visitTypeInsn (Opcodes.CHECKCAST, quotedType.getInternalName ());
		mv.visitFieldInsn (Opcodes.PUTFIELD, classInternalName, reference.getName (), typeDescriptor);
	    }
	}
    }

    private void loadQuotedData (final MethodVisitor mv, final Symbol reference)
    {
	final Type classLoaderType = Type.getType (getClass ());
	final String classLoaderInternalName = classLoaderType.getInternalName ();
	final String mapMethodDescriptor = Type.getMethodDescriptor (OBJECT_TYPE, OBJECT_TYPE);
	mv.visitVarInsn (Opcodes.ALOAD, 0);
	mv.visitInsn (Opcodes.DUP);
	mv.visitMethodInsn (Opcodes.INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
	mv.visitMethodInsn (Opcodes.INVOKEVIRTUAL, "java/lang/Class", "getClassLoader", "()Ljava/lang/ClassLoader;", false);
	mv.visitTypeInsn (Opcodes.CHECKCAST, classLoaderInternalName);
	mv.visitMethodInsn (Opcodes.INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;", false);

	mv.visitLdcInsn (reference.getName ());
	mv.visitMethodInsn (Opcodes.INVOKEINTERFACE, "java/util/Map", "get", mapMethodDescriptor, true);
    }
}
