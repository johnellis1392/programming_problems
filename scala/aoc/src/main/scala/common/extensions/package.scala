package common

package object extensions {
  
  extension [T](self: T)
    def ensure(pred: T => Boolean): T =
      assert(pred(self)); self

    def |>[R](fn: T => R): R = fn(self)

    def let[R](fn: T => R): R = fn(self)


  extension [T](self: Iterable[T])
    def minOf[R](fn: T => R)(using Ordering[R]): R = self.map(fn).min

    def pairwise: Iterable[(T, T)] = self.zip(self.drop(1))


  extension [K, V](self: Map[K, V])
    def putIfAbsent(k: K, v: V): Map[K, V] = self.updatedWith(k) {
      case None => Some(v)
      case s@Some(_) => s
    }

    def compute(k: K, v: V)(fn: (Option[V], V) => V): Map[K, V] =
      self.updatedWith(k) { w => Some(fn(w, v)) }

    def computeIfPresent(k: K, v: V)(fn: (V, V) => V): Map[K, V] =
      self.updatedWith(k) {
        case None => Some(v)
        case Some(w) => Some(fn(w, v))
      }

  extension [K, V](self: Iterable[(K, V)])
    def toMapColliding(collisionHandler: (V, V) => V): Map[K, V] =
      self.foldLeft(Map[K, V]()) { case (m, (k, v)) =>
        m.computeIfPresent(k, v)(collisionHandler)
      }


}
