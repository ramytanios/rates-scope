package lib

import lib.syntax.*
import lib.dtos.BusinessDayConvention.*

trait SettlementRule[T]:

  def fixingAt(settlementAt: T): T

  def settlementAt(fixingAt: T): T

object SettlementRule:

  def simpleRule[T: DateLike](days: Int, fixingCalendar: Calendar[T]): SettlementRule[T] =
    new SettlementRule[T]:

      def settlementAt(fixingAt: T): T = fixingCalendar.addBusinessDays(fixingAt, days)

      def fixingAt(settlementAt: T): T =
        var _fixingAt = settlementAt
        var _settlementAt = this.settlementAt(_fixingAt)
        while _settlementAt > settlementAt do
          _fixingAt = Preceding.adjust(_fixingAt - 1, fixingCalendar)
          _settlementAt = this.settlementAt(_fixingAt)
        if _settlementAt < settlementAt then
          _fixingAt = Following.adjust(_fixingAt + 1, fixingCalendar)
          _settlementAt = this.settlementAt(_fixingAt)
        _fixingAt
