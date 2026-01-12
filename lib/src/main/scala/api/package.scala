package lib

package object api:

  def toDayCounter(dc: dtos.DayCounter): lib.DayCounter =
    dc match
      case dtos.DayCounter.Act360 => lib.DayCounter.Act360
      case dtos.DayCounter.Act365 => lib.DayCounter.Act365

  extension (vu: dtos.VolUnit)
    def toUnit: Double => Double =
      vu match
        case dtos.VolUnit.BpPerYear => _ * 10000

    def fromUnit: Double => Double =
      vu match
        case dtos.VolUnit.BpPerYear => _ / 10000
