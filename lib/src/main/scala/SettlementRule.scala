package lib

import java.time.LocalDate

trait SettlementRule:

  def fixingDate(date: LocalDate, calendar: Calendar): LocalDate

object SettlementRule:

  def simpleRule(spotLag: Long): SettlementRule =
    new SettlementRule:
      override def fixingDate(date: LocalDate, calendar: Calendar): LocalDate =
        calendar.addBusinessDays(date, -spotLag)
