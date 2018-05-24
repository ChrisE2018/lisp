
package lisp.util;

@FunctionalInterface
public interface ThrowingSupplier<T>
{
    T get () throws Exception;
}
