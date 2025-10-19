package lib

trait SettlementRule[T]:

  def calendar: Calendar[T]

  def fixingDate(of: T): T

object SettlementRule:

  def simpleRule[T](spotLag: Long)(using cal: Calendar[T]): SettlementRule[T] =
    new SettlementRule:
      def calendar: Calendar[T] = cal
      def fixingDate(of: T): T = calendar.addBusinessDays(of, -spotLag)
