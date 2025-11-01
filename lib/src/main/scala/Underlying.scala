package lib

import lib.dtos.*
import lib.quantities.Tenor

trait Underlying[T]:

  def currency: Currency

  def tenor: Tenor

  def forward: Forward[T]

  def bdConvention: BusinessDayConvention

  def calendar: Calendar[T]

  def interestPeriod(fixingAt: T): (T, T)
