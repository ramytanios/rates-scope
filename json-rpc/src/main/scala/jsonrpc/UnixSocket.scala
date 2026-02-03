package jsonrpc

import cats.effect.*
import cats.effect.std.Queue
import fs2.io.net.unixsocket.*
import fs2.text
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.log4cats.*

import java.nio.file.Paths

object UnixSocket extends IOApp.Simple:

  given LoggerFactory[IO] = slf4j.Slf4jFactory.create[IO]

  def impl(L: Logger[IO]): IO[Unit] =

    val socketPath = Paths.get("/tmp/rates-scope.sock")

    UnixSockets[IO].server(UnixSocketAddress(socketPath))
      .parEvalMapUnbounded: unixSocket =>
        Queue.bounded[IO, JsonRpc.Request](1000).flatMap: Q =>

          val read = unixSocket
            .reads
            .through(text.utf8.decode)
            .through(text.lines)
            .map(decode[JsonRpc.Request](_))
            .evalMapFilter:
              case Left(err) => L.warn(err)(s"failed to decode socket message") *> IO.pure(None)
              case Right(js) => IO.pure(Some(js))
            .evalMap(Q.offer)

          val write = fs2.Stream.fromQueueUnterminated(Q)
            .parEvalMap(128)(request => IO(Handler(request)))
            .map(_.asJson.noSpaces)
            .intersperse("\n")
            .through(text.utf8.encode)
            .through(unixSocket.writes)

          read.concurrently(write).compile.drain
      .compile
      .drain

  override def run: IO[Unit] = LoggerFactory[IO].create.flatMap(impl)
