
package lisp.cc;

import java.io.*;
import java.util.Map;
import java.util.logging.*;

import org.objectweb.asm.*;

import lisp.*;
import lisp.Package;
import lisp.Symbol;
import lisp.cc1.CompileLoader_v1;
import lisp.cc2.CompileLoader_v2;
import lisp.cc3.CompileClassAdaptor_v3;
import lisp.cc4.*;
import lisp.util.LogString;

public class CompilerFactory
{
    private static final Logger LOGGER = Logger.getLogger (CompilerFactory.class.getName ());

    enum Version
    {
	V1, V2, V3, V4;
    }

    private static Version DEFAULT_VERSION = Version.V3;

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
		final CompileLoader result = new CompileLoader ();
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
		final CompileLoader result = new CompileLoader ();
		// set class visitor to null to disable actual bytecode generation. Sometimes this
		// allows the bytecode to be printed before an error that would otherwise be
		// difficult to debug.
		// result.setClassVisitor (null);
		result.setClassReader (null);
		result.setClassType (classType);
		ClassVisitor cv = result.getClassVisitor ();
		// (setLoggerLevel "lisp.cc.PrintBytecodeClassAdaptor" "INFO")
		final Logger pbl = Logger.getLogger (PrintBytecodeClassAdaptor.class.getName ());
		if (pbl.isLoggable (Level.INFO))
		{
		    cv = new PrintBytecodeClassAdaptor (Compiler.ASM_VERSION, cv, new StringWriter ());
		}
		if (Symbol.named ("system", "optimize").getBooleanValue (true))
		{
		    cv = new Optimizer (Compiler.ASM_VERSION, cv);
		}
		cv = new TreeCompiler (cv, result, returnType, methodName, methodArgs, methodBody);
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
