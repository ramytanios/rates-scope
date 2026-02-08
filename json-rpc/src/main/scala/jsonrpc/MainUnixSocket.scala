package jsonrpc

import cats.effect.*
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.net.Socket
import fs2.io.net.unixsocket.*
import fs2.text
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.log4cats.*

import java.util.UUID

object MainUnixSocket extends IOApp.Simple:

  case class Settings(
      socketPath: Path = Path("/tmp/rates-scope.sock"),
      clientsConcurrency: Int = 16,
      requestsConcurrency: Int = 256
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
      settings: Settings,
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
        .parEvalMap(settings.requestsConcurrency):
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

  def impl(settings: Settings, L: Logger[IO]): IO[Unit] =
    L.info(s"listening on socket ${settings.socketPath}") *>
      UnixSockets[IO].server(UnixSocketAddress(settings.socketPath.toString))
        .map: socket =>
          fs2.Stream.eval(IO.randomUUID)
            .evalMap(handleConnection(_, socket, settings, L))
        .parJoin(settings.clientsConcurrency)
        .compile
        .drain
        .guarantee(cleanup(settings.socketPath))

  override def run: IO[Unit] =
    slf4j.Slf4jFactory.create[IO].create.flatMap(impl(Settings(), _))
