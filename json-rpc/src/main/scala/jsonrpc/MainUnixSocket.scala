package jsonrpc

import cats.effect.*
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.net.Socket
import fs2.io.net.unixsocket.*
import fs2.text
import io.circe.parser.*
import io.circe.syntax.*
import cats.syntax.all.*
import org.typelevel.log4cats.*

import java.util.UUID
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.Opts

object MainUnixSocket extends CommandIOApp("rates-scope", "Rates jRPC"):

  override def main: Opts[IO[ExitCode]] =
    cliArgs.map: cli =>
      slf4j.Slf4jFactory.create[IO].create.flatMap(
        impl(cli, _)
      ).handleError(_ => ExitCode.Error).as(ExitCode.Success)

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

  def cleanup(path: Path): IO[Unit] = Files[IO].deleteIfExists(path).void

  /**
   * Assumes each client guarantees each JSON-RPC request
   * is newline-delimited and contains no newlines inside the JSON.
   */
  def handleConnection(
      uuid: UUID,
      conn: Socket[IO],
      cli: CliArgs,
      L: Logger[IO]
  ): IO[Unit] =
    L.info(s"new client connection $uuid") *>
      conn
        .reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalTap(line => L.debug(s"received line $line"))
        .map(decode[JsonRpc.Request](_))
        .parEvalMap(cli.requestsConcurrency):
          case Left(th)       => IO.pure(JsonRpc.error(JsonRpc.ErrorCode.ParseError, th.getMessage))
          case Right(request) => IO.pure(Handler(request))
        .map(_.asJson.noSpaces ++ EOL)
        .evalTap(line => L.debug(s"writing line $line"))
        .through(text.utf8.encode)
        .through(conn.writes)
        .compile
        .drain
        .handleErrorWith(th => L.error(th)(s"connection $uuid handler failed"))
        .guaranteeCase:
          case Outcome.Succeeded(_) => L.warn(s"reads $uuid: succeeded/EOF")
          case Outcome.Errored(e)   => L.warn(s"reads $uuid: errored, $e")
          case Outcome.Canceled()   => L.warn(s"reads $uuid: canceled")

  def impl(cli: CliArgs, L: Logger[IO]): IO[Unit] =
    L.info(s"listening on socket ${cli.socketPath}") *>
      UnixSockets[IO].server(UnixSocketAddress(cli.socketPath.toString))
        .map: socket =>
          fs2.Stream.eval(IO.randomUUID)
            .evalMap(handleConnection(_, socket, cli, L))
        .parJoin(cli.clientsConcurrency)
        .compile
        .drain
        .guarantee(cleanup(cli.socketPath))
