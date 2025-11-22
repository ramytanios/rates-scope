package lib

import lib.syntax.*

trait Detachment[T]:

  def isDetached(tRef: T, t: T): Boolean

object Detachment:

  def default[T: DateLike]: Detachment[T] =
    (tRef: T, t: T) => t >= tRef
