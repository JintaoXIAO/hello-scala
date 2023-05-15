package haskell

trait Monoid[T] {
  def mempty: T
  def mappend: (T, T) => T

  def mconcat(ts: List[T]): T =
    ts.foldRight(mempty)(mappend)
}
