package lib

trait SwapLike[T] extends Underlying[T]:

  def fixedDayCounter: DayCounter

  def discountCurve: YieldCurve[T]

  def fixedSchedule(from: T, to: T): IndexedSeq[FixedCoupon[T]]
