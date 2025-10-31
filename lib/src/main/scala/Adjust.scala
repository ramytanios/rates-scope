package lib

import lib.dtos.BusinessDayConvention.*
import lib.syntax.*

object Adjust:

  def apply[T: DateLike](t: T, convention: dtos.BusinessDayConvention, calendar: Calendar[T]): T =

    convention match
      case Following =>
        var res = t; while !calendar.isBusinessDay(res) do res = res + 1; res
      case Preceding =>
        var res = t; while !calendar.isBusinessDay(res) do res = res - 1; res
      case ModifiedFollowing =>
        val tn = apply(t, Following, calendar)
        if DateLike[T].onSameMonth(t, tn) then tn else apply(t - 1, Preceding, calendar)

  trait Syntax:

    extension [T: DateLike](c: dtos.BusinessDayConvention)
      def adjust(t: T, calendar: Calendar[T]): T = Adjust(t, c, calendar)
