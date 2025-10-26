package lib

import cats.kernel.Monoid

trait CollectionSyntax:

  // `cats` only provides this syntax for concrete types such as `Vector`
  extension [V](s: IndexedSeq[V])
    def foldMap[B: Monoid](f: V => B) =
      s.foldLeft(Monoid[B].empty): (acc, v) =>
        Monoid[B].combine(acc, f(v))
