package lib

import org.apache.commons.math3.distribution.NormalDistribution

object normal:

  private val nd = NormalDistribution()

  def cdf(x: Double): Double = nd.cumulativeProbability(x)

  def pdf(x: Double): Double = nd.density(x)

  def inverseCdf(x: Double): Double = nd.inverseCumulativeProbability(x)
