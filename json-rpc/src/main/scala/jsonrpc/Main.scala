package jsonrpc

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Queue
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.*
import scala.math.max

object Main extends IOApp.Simple:

  override def run: IO[Unit] = IO.unit
