
package lisp.cc;

import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.lang.*;
import lisp.lang.Symbol;
import lisp.util.*;

public class QuotedDataReader implements QuotedData
{
    private static final Logger LOGGER = Logger.getLogger (LispQuotedClassLoader.class.getName ());
    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private static JavaName javaName = new JavaName ();

    /** Fields containing quoted data required by the compilation class. */
    private final Map<Symbol, Object> quotedReferences = new LinkedHashMap<Symbol, Object> ();

    /** References to symbols that must be available to the bytecode. */
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

    public static Object readQuotedData (final String data)
    {
	try
	{
	    final LispReader reader = new LispReader ();
	    return reader.read (new LispInputStream (data));
	}
	catch (final IOException e)
	{
	    e.printStackTrace ();
	}
	return null;
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
	// FIXME need to be sure that round trip through read returns an equal value.
	final Symbol reference = QUOTE_SYMBOL.gensym ();
	quotedReferences.put (reference, quoted);
	return reference;
    }

    /** Get the saved quoted data. */
    @Override
    public Map<Symbol, Object> getQuotedData ()
    {
	return quotedReferences;
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

	if (!quotedReferences.isEmpty ())
	{
	    final String myInternalName = Type.getType (getClass ()).getInternalName ();
	    // Create initialization code for all required quoted data.
	    for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	    {
		// (define foo () (quote bar))
		final Symbol reference = entry.getKey ();
		final Object quoted = entry.getValue ();

		mv.visitVarInsn (Opcodes.ALOAD, 0);
		mv.visitLdcInsn (quoted.toString ());
		mv.visitMethodInsn (Opcodes.INVOKESTATIC, myInternalName, "readQuotedData",
		        "(Ljava/lang/String;)Ljava/lang/Object;", false);

		final Type quotedType = Type.getType (quoted.getClass ());
		final String typeDescriptor = quotedType.getDescriptor ();
		mv.visitTypeInsn (Opcodes.CHECKCAST, quotedType.getInternalName ());
		mv.visitFieldInsn (Opcodes.PUTFIELD, classInternalName, reference.getName (), typeDescriptor);
	    }
	}
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
