import java.time.LocalDate
import java.time.temporal.ChronoUnit

package object lib:

  def days(from: LocalDate, to: LocalDate): Long = ChronoUnit.DAYS.between(from, to)
