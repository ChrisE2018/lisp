
package lisp.cc;

import java.util.Map;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.ClassNode;

import lisp.lang.Symbol;

public class QuotedDataStub implements QuotedData
{
    @Override
    public void addSymbolReference (final Symbol symbol)
    {
	throw new UnsupportedOperationException ();
    }

    @Override
    public Symbol addQuotedConstant (final Object quoted)
    {
	throw new UnsupportedOperationException ();
    }

    @Override
    public Map<Symbol, Object> getQuotedData ()
    {
	return null;
    }

    @Override
    public void addRequiredFields (final ClassNode cn)
    {
    }

    @Override
    public void addHiddenConstructorSteps (final Type classType, final MethodVisitor mv)
    {
    }
}
