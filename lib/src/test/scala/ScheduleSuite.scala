package lib

import lib.dtos.*
import lib.literals.*
import lib.quantities.Tenor

class ScheduleSuite extends munit.FunSuite:

  test("schedule generation"):

    val s = Schedule(
      d"2025-10-16",
      d"2025-10-26",
      Tenor.`1D`,
      Calendar.all,
      BusinessDayConvention.Following,
      StubConvention.Short,
      Direction.Forward
    )

    assertEquals(s.head, d"2025-10-16", s"${s.head}")
    assertEquals(s.last, d"2025-10-26", s"${s.last}")
    assertEquals(s.size, 11)

  test("stub"):

    StubConvention.values.foreach: stub =>
      val s = Schedule(
        d"2025-10-16",
        d"2025-10-26",
        Tenor.days(3),
        Calendar.all,
        BusinessDayConvention.Following,
        stub,
        Direction.Forward
      )

      stub match
        case StubConvention.Short => assertEquals(s.size, 5, s"stub=$stub")
        case StubConvention.Long  => assertEquals(s.size, 4, s"stub=$stub")

  test("direction"):

    Direction.values.foreach: direction =>
      val s = Schedule(
        d"2025-10-16",
        d"2025-10-26",
        Tenor.days(3),
        Calendar.all,
        BusinessDayConvention.Following,
        StubConvention.Short,
        direction
      )

      assertEquals(s.size, 5, s"direction=$direction")

  test("duplicates"):

    val s = Schedule(
      d"2025-08-29",
      d"2025-09-05",
      Tenor.`1D`,
      Calendar.fromHolidays(Nil),
      BusinessDayConvention.Following,
      StubConvention.Long,
      Direction.Forward
    )

    assertEquals(s.size, 6)
