package lib.dtos

import cats.Show
import io.circe.*
import io.circe.syntax.*

import scala.util.Try

case class Tenor(length: Int, unit: Tenor.Unit)

object Tenor:

  enum Unit:
    case Day, Week, Month, Year

  object Unit:

    def parse(u: String): Option[Unit] =
      u match
        case "d" | "D" => Some(Unit.Day)
        case "m" | "M" => Some(Unit.Month)
        case "w" | "W" => Some(Unit.Week)
        case "y" | "Y" => Some(Unit.Year)
        case _         => None

  val regex = """^(\d+)\s*([DWMY])$""".r

  val `1D`: Tenor = Tenor(1, Unit.Day)
  val `3M`: Tenor = Tenor(3, Unit.Month)
  val `6M`: Tenor = Tenor(6, Unit.Month)
  val `1Y`: Tenor = Tenor(1, Unit.Year)
  val `2Y`: Tenor = Tenor(2, Unit.Year)
  val `10Y`: Tenor = Tenor(10, Unit.Year)

  def parse(s: String): Option[Tenor] =
    s match
      case regex(len, unit) =>
        Unit.parse(unit).flatMap: u =>
          len.toIntOption.map: l =>
            Tenor(l, u)
      case _ => None

  def parseUnsafe(s: String): Tenor = parse(s).get

  given Show[Tenor] = Show.show: t =>
    t.unit match
      case Unit.Day   => s"${t.length}D"
      case Unit.Week  => s"${t.length}W"
      case Unit.Month => s"${t.length}M"
      case Unit.Year  => s"${t.length}Y"

  given Encoder[Tenor] = Encoder.instance(s => Show[Tenor].show(s).asJson)

  given Decoder[Tenor] = Decoder.decodeString.emapTry(s => Try(Tenor.parseUnsafe(s)))

  given KeyEncoder[Tenor] = KeyEncoder.instance(Show[Tenor].show)

  given KeyDecoder[Tenor] = KeyDecoder.instance(Tenor.parse)
