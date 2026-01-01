package lib

import lib.literals.*
import lib.quantities.Tenor

class ScheduleSuite extends munit.FunSuite:

  test("schedule generation"):

    val s = Schedule(
      d"2025-10-16",
      d"2025-10-26",
      Tenor.`1D`,
      Calendar.all,
      dtos.BusinessDayConvention.Following,
      dtos.StubConvention.Short,
      dtos.Direction.Forward
    )

    assertEquals(s.head, d"2025-10-16", s"${s.head}")
    assertEquals(s.last, d"2025-10-26", s"${s.last}")
    assertEquals(s.size, 11)

  test("stub"):

    dtos.StubConvention.values.foreach: stub =>
      val s = Schedule(
        d"2025-10-16",
        d"2025-10-26",
        Tenor.days(3),
        Calendar.all,
        dtos.BusinessDayConvention.Following,
        stub,
        dtos.Direction.Forward
      )

      stub match
        case dtos.StubConvention.Short => assertEquals(s.size, 5, s"stub=$stub")
        case dtos.StubConvention.Long  => assertEquals(s.size, 4, s"stub=$stub")

  test("direction"):

    dtos.Direction.values.foreach: direction =>
      val s = Schedule(
        d"2025-10-16",
        d"2025-10-26",
        Tenor.days(3),
        Calendar.all,
        dtos.BusinessDayConvention.Following,
        dtos.StubConvention.Short,
        direction
      )

      assertEquals(s.size, 5, s"direction=$direction")

  test("duplicates"):

    val s = Schedule(
      d"2025-08-29",
      d"2025-09-05",
      Tenor.`1D`,
      Calendar.fromHolidays(IndexedSeq(d"2025-08-30", d"2025-08-31")),
      dtos.BusinessDayConvention.Following,
      dtos.StubConvention.Long,
      dtos.Direction.Forward
    )

    assertEquals(s.size, 6)
