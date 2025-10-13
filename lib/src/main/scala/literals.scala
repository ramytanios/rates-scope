package lib

import cats.syntax.all.*
import org.typelevel.literally.Literally

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object literals:
  extension (inline ctx: StringContext)
    inline def d(inline args: Any*): LocalDate =
      ${ LocalDateLiteral('ctx, 'args) }

  object LocalDateLiteral extends Literally[LocalDate]:
    private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    def validate(s: String)(using Quotes) =
      Either
        .catchNonFatal(LocalDate.parse(s, formatter))
        .leftMap: err =>
          s"Failed to parse string to `LocalDate`: $err"
        .map: _ =>
          '{ LocalDate.parse(${ Expr(s) }, formatter) }
