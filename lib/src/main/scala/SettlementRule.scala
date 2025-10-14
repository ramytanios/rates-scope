package lib

import java.time.LocalDate

trait SettlementRule:

  def calendar: Calendar

  def fixingDate(date: LocalDate): LocalDate

object SettlementRule:

  def simpleRule(spotLag: Long)(using cal: Calendar): SettlementRule =
    new SettlementRule:
      def calendar: Calendar = cal
      def fixingDate(date: LocalDate): LocalDate = calendar.addBusinessDays(date, -spotLag)
