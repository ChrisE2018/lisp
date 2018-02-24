
package lisp;

public enum ListKind
{
    BRACE ('{', '}'), BRACKET ('[', ']'), PAREN ('(', ')');

    private final char openChar;
    private final char closeChar;

    private ListKind (final char openChar, final char closeChar)
    {
	this.openChar = openChar;
	this.closeChar = closeChar;
    }

    public char getOpenChar ()
    {
	return this.openChar;
    }

    public char getCloseChar ()
    {
	return this.closeChar;
    }
}
