
package lisp;

@FunctionalInterface
public interface ThrowingSupplier<T>
{
    T get () throws Exception;
}
