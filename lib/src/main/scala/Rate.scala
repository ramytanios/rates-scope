package lib

import java.time.LocalDate

trait Rate:

  // or type class ?
  def forward(date: LocalDate): Either[Error, Double]
