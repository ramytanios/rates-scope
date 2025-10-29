package lib

import lib.dtos.BusinessDayConvention
import lib.dtos.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.Tenor

class CalendarSuite extends munit.FunSuite:

  val holidays = Seq(
    d"2025-12-25", // Christmas Day
    d"2026-01-01" // New Year's Day
  )

  val calendar = Calendar.fromHolidays(holidays)

  test("business calendar"):

    assert(calendar.isBusinessDay(d"2025-12-25") == false)

    assertEquals(
      calendar.addBusinessDays(d"2025-12-24", 1),
      d"2025-12-26"
    )

    assertEquals(
      calendar.addBusinessPeriod(d"2025-12-24", Tenor.days(1))(using BusinessDayConvention.Following),
      d"2025-12-26"
    )

    assertEquals(
      calendar.countBusinessDays(d"2025-12-24", d"2025-12-27"),
      2L
    )

  test("business day convention"):

    assertEquals(
      calendar.addBusinessPeriod(d"2025-12-31", Tenor.days(1))(using Following),
      d"2026-01-02",
      "Following"
    )

    assertEquals(
      calendar.addBusinessPeriod(d"2025-12-31", Tenor.days(1))(using Preceding),
      d"2025-12-31",
      "Preceding"
    )

    assertEquals(
      calendar.addBusinessPeriod(d"2025-12-31", Tenor.days(1))(using ModifiedFollowing),
      d"2026-01-02",
      "ModifiedFollowing"
    )
