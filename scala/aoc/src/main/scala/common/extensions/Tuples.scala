package common.extensions

extension [T](self: (T, T))
  def both[R](fn: T => R): (R, R) = (fn(self._1), fn(self._2))
  def unzip[R](fn: (T, T) => R): R = fn(self._1, self._2)
