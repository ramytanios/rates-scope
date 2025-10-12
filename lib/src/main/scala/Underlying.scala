package lib

import java.time.LocalDate

trait Underlying:

  def forward(t: LocalDate)(using Market): Either[Error, Double]
