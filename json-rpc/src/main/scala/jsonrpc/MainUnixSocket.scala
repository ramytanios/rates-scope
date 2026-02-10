package jsonrpc

import cats.effect.*
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Path
import fs2.io.net.Socket
import fs2.io.net.unixsocket.*
import fs2.text
import io.circe.parser.*
import io.circe.syntax.*

import java.util.UUID

object MainUnixSocket extends CommandIOApp("rates-scope", "Rates jRPC"):

  override def main: Opts[IO[ExitCode]] =
    cliArgs.map: cli =>
      impl(cli).handleError(_ => ExitCode.Error).as(ExitCode.Success)

  private val socketPath: Opts[Path] =
    Opts.option[String]("socket-path", "Socket path").map(Path.apply)
      .withDefault(Path("/tmp/rates-scope.sock"))

  private val clientsConcurrency: Opts[Int] =
    Opts.option[Int]("clients-concurrency", "Clients concurrency")
      .withDefault(16)

  private val requestsConcurrency: Opts[Int] =
    Opts.option[Int]("requests-concurrency", "Requests concurrency")
      .withDefault(256)

  private val cliArgs: Opts[CliArgs] =
    (socketPath, clientsConcurrency, requestsConcurrency).mapN(CliArgs.apply)

  final case class CliArgs(
      socketPath: Path,
      clientsConcurrency: Int,
      requestsConcurrency: Int
  )

  val EOL = "\n"

  /**
   * Assumes each client guarantees each JSON-RPC request
   * is newline-delimited and contains no newlines inside the JSON.
   */
  def handleConnection(uuid: UUID, conn: Socket[IO], cli: CliArgs): IO[Unit] =
    IO.println(s"new client connection $uuid") *>
      conn
        .reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .map(decode[JsonRpc.Request](_))
        .parEvalMap(cli.requestsConcurrency):
          case Left(th)       => IO.pure(JsonRpc.error(JsonRpc.ErrorCode.ParseError, th.getMessage))
          case Right(request) => IO.pure(Handler(request))
        .map(_.asJson.noSpaces ++ EOL)
        .through(text.utf8.encode)
        .through(conn.writes)
        .compile
        .drain
        .handleErrorWith(th => IO.println(s"connection $uuid handler failed: $th"))
        .guaranteeCase:
          case Outcome.Succeeded(_) => IO.println(s"reads $uuid: succeeded/EOF")
          case Outcome.Errored(e)   => IO.println(s"reads $uuid: errored, $e")
          case Outcome.Canceled()   => IO.println(s"reads $uuid: canceled")

  def impl(cli: CliArgs): IO[Unit] =
    IO.println(s"listening on socket ${cli.socketPath}") *>
      UnixSockets[IO].server(
        UnixSocketAddress(cli.socketPath.toString),
        deleteIfExists = true,
        deleteOnClose = true
      )
        .map: socket =>
          fs2.Stream.eval(IO.randomUUID)
            .evalMap(handleConnection(_, socket, cli))
        .parJoin(cli.clientsConcurrency)
        .compile
        .drain
