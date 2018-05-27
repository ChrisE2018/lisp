
package lisp.cc;

import java.io.*;

import org.objectweb.asm.*;

import lisp.*;
import lisp.Symbol;

public class CompilerFactory
{
    /**
     * Flag to control display of bytecode after compile. <br/>
     * [TODO] Should use logger configuration instead of hard-coded flags.
     */
    private static final String SHOW_BYTECODE = "showBytecode";

    enum Version
    {
	V1, V1_5, V2;
    }

    private static Version DEFAULT_VERSION = Version.V2;

    private Version version = Version.V1;

    public CompilerFactory ()
    {
	version = DEFAULT_VERSION;
    }

    public CompilerFactory (final Version version)
    {
	this.version = version;
    }

    public Compiler getCompiler (final Class<?> returnType, final String methodName, final LispList methodArgs,
            final LispList methodBody) throws IOException
    {
	switch (version)
	{
	    case V1:
	    {
		return new CompileLoader_v1 (returnType, methodName, methodArgs, methodBody);
	    }
	    case V1_5:
	    {
		final CompileLoader_v1_5 result = new CompileLoader_v1_5 (returnType, methodName, methodArgs, methodBody);
		return result;
	    }
	    case V2:
	    {
		final Symbol showBytecodeSymbol = PackageFactory.getSystemPackage ().internSymbol (SHOW_BYTECODE);
		final boolean showBytecode = showBytecodeSymbol.getValue (false) != Boolean.FALSE;
		final CompileLoader_v2 result = new CompileLoader_v2 (returnType, methodName, methodArgs, methodBody);
		if (showBytecode)
		{
		    final ClassVisitor cv = result.getClassVisitor ();
		    result.setClassVisitor (new PrintBytecodeClassAdaptor (Opcodes.ASM5, cv, new StringWriter ()));
		}
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
