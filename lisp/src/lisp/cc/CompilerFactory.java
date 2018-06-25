
package lisp.cc;

import java.io.*;
import java.util.Map;
import java.util.logging.*;

import org.objectweb.asm.*;

import lisp.asm.PrintBytecodeClassAdaptor;
import lisp.cc1.CompileLoader_v1;
import lisp.cc2.CompileLoader_v2;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.lang.Symbol;
import lisp.util.LogString;

public class CompilerFactory
{
    private static final Logger LOGGER = Logger.getLogger (CompilerFactory.class.getName ());

    enum Version
    {
	V1, V2, V3, V4;
    }

    private static Version DEFAULT_VERSION = Version.V4;

    private final Package systemPackage = PackageFactory.getSystemPackage ();
    /**
     * Flag to control display of bytecode after compile. <br/>
     * CONSIDER Should use logger configuration instead of hard-coded flags.
     */
    private final Symbol showBytecodeSymbol = systemPackage.internSymbol ("showBytecode");
    private final Symbol compilerVersionSymbol = systemPackage.internSymbol ("compilerVersion");

    public Compiler getCompiler (final Class<?> returnType, final String methodName, final LispList methodArgs,
            final LispList methodBody) throws IOException
    {
	final String versionName = compilerVersionSymbol.getStringValue (DEFAULT_VERSION.toString ());
	final Version version = Version.valueOf (versionName);
	LOGGER.fine (new LogString ("Using compiler version %s", version));
	switch (version)
	{
	    case V1:
	    {
		return new CompileLoader_v1 (returnType, methodName, methodArgs, methodBody);
	    }
	    case V2:
	    {
		final boolean showBytecode = showBytecodeSymbol.getValue (false) != Boolean.FALSE;
		final CompileLoader_v2 result = new CompileLoader_v2 (returnType, methodName, methodArgs, methodBody);
		if (showBytecode)
		{
		    final ClassVisitor cv = result.getClassVisitor ();
		    result.setClassVisitor (new PrintBytecodeClassAdaptor (Compiler.ASM_VERSION, cv, new StringWriter ()));
		}
		return result;
	    }
	    case V3:
	    {
		final CompileLoaderV3 result = new CompileLoaderV3 ();
		ClassVisitor cv = result.getClassVisitor ();
		final boolean showBytecode = showBytecodeSymbol.getValue (false) != Boolean.FALSE;
		if (showBytecode)
		{
		    cv = new PrintBytecodeClassAdaptor (Compiler.ASM_VERSION, cv, new StringWriter ());
		}
		final Map<String, Object> quotedReferences = result.getQuotedReferences ();
		cv = new CompileClassAdaptor_v3 (cv, result.getClassType (), returnType, methodName, methodArgs, methodBody,
		        quotedReferences);
		result.setClassVisitor (cv);
		return result;
	    }
	    case V4:
	    {
		final Type classType = Type.getType ("Llisp/cc/Foobar;");
		final CompileLoaderV4 result = new CompileLoaderV4 ();
		// set class visitor to null to disable actual bytecode generation. Sometimes this
		// allows the bytecode to be printed before an error that would otherwise be
		// difficult to debug.
		// result.setClassVisitor (null);
		result.setClassReader (null);
		result.setClassType (classType);
		ClassVisitor cv = result.getClassVisitor ();
		final Logger pbl = Logger.getLogger (PrintBytecodeClassAdaptor.class.getName ());
		if (pbl.isLoggable (Level.INFO))
		{
		    cv = new PrintBytecodeClassAdaptor (Compiler.ASM_VERSION, cv, new StringWriter ());
		}
		if (Symbol.named ("lisp.lang", "optimize").getBooleanValue (true))
		{
		    // This won't work because it assumes the ClassWriter has already determined the
		    // frames.
		    // Need to make that run, then read the class back and finally load the
		    // bytecode.
		    // cv = new OptimizeClassVisitor (Compiler.ASM_VERSION, cv);
		    cv = new Optimizer (Compiler.ASM_VERSION, cv);
		}
		cv = new TreeCompiler (Compiler.ASM_VERSION, cv, result, returnType, methodName, methodArgs, methodBody);
		result.setClassVisitor (cv);
		return result;
	    }
	    default:
	    {
		throw new UnsupportedOperationException ("Invalid compiler version " + version);
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
