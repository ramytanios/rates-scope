package lib

import lib.syntax.*

trait Detachment[T]:

  def isDetached(t: T): Boolean

object Detachment:

  def fixedDetachment[T: DateLike](detachedAt: T): Detachment[T] =
    (t: T) => t >= detachedAt
