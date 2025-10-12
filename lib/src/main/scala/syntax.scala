package lib

import java.time.LocalDate
import lib.quantities.*

object syntax:

  extension (d: LocalDate)
    def +(t: Tenor) = d.plus(t.toPeriod)
    def <(other: LocalDate) = d.isBefore(other)
    def <=(other: LocalDate) = d.isBefore(other) && d.isEqual(other)
