package lib

import java.time.LocalDate

trait Underlying:

  def forward(date: LocalDate)(using Market): Either[Error, Double]
