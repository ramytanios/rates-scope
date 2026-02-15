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

  override def main: Opts[IO[ExitCode]] = cliArgs.map: args =>
    impl(args).as(ExitCode.Success).handleErrorWith(th =>
      warn(s"Exiting app with error $th").as(ExitCode.Error)
    )

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
    info(s"New client connection ${uuid.toString().bold}") *>
      conn
        .reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .map(decode[JsonRpc.Request](_))
        .parEvalMap(cli.requestsConcurrency):
          case Left(th) => error(s"Failed to decode message: $th") *> IO.pure(
              JsonRpc.error(JsonRpc.ErrorCode.ParseError, th.getMessage)
            )
          case Right(request) =>
            info(s"Received request with id=${request.id}, method=${request.method}") *> IO.pure(
              Handler(request)
            )
        .map(_.asJson.noSpaces ++ EOL)
        .through(text.utf8.encode)
        .through(conn.writes)
        .compile
        .drain
        .handleErrorWith(th => warn(s"Connection $uuid handler failed: $th"))
        .guaranteeCase:
          case Outcome.Succeeded(_) => info(s"Reads $uuid: succeeded/EOF")
          case Outcome.Errored(e)   => warn(s"Reads $uuid: errored, $e")
          case Outcome.Canceled()   => warn(s"Reads $uuid: canceled")

  def impl(cli: CliArgs): IO[Unit] =
    info(s"Listening on socket ${cli.socketPath.toString.bold}") *>
      UnixSockets[IO].server(
        UnixSocketAddress(cli.socketPath.toString),
        deleteIfExists = true,
        deleteOnClose = true
      )
        .map(socket =>
          fs2.Stream.eval(IO.randomUUID)
            .evalMap(handleConnection(_, socket, cli))
        )
        .parJoin(cli.clientsConcurrency)
        .compile
        .drain
