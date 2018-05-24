
package lisp.util;

@FunctionalInterface
public interface ThrowingConsumer<T>
{
    public void accept (final T t) throws Exception;
}
