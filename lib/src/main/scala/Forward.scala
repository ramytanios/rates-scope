package lib

import java.time.LocalDate

trait Forward:

  def apply(t: LocalDate): Double
