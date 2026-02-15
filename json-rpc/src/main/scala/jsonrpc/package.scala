import cats.effect.*

import scala.io.AnsiColor

package object jsonrpc:

  def info(text: String): IO[Unit] = IO.println(
    s"${"[INFO]".bold.colored(AnsiColor.CYAN)} $text"
  )

  def warn(text: String): IO[Unit] = IO.println(
    s"${"[WARN]".bold.colored(AnsiColor.YELLOW)} $text"
  )

  def error(text: String): IO[Unit] = IO.println(
    s"${"[ERROR]".bold.colored(AnsiColor.RED)} $text"
  )

  extension (text: String)
    def bold: String = AnsiColor.BOLD ++ text ++ AnsiColor.RESET
    def colored(color: String): String = color ++ text ++ AnsiColor.RESET
