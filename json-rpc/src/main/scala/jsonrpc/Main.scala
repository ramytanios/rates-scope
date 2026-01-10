package jsonrpc

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.typelevel.log4cats.*
import org.typelevel.log4cats.syntax.*
import org.http4s.server.middleware.Logger

object Main extends IOApp.Simple:

  case class HttpServerException(msg: String) extends RuntimeException(msg)

  override def run: IO[Unit] =

    given LoggerFactory[IO] = slf4j.Slf4jFactory.create[IO]

    val host = "localhost"
    val port = 8090

    val app = HttpRoutes.of[IO]:
      case request @ POST -> Root / "rpc" =>
        request.as[JsonRpc.Request].attempt.flatMap:
          case Left(th) => Ok(JsonRpc.error(JsonRpc.ErrorCode.ParseError, th.getMessage))
          case Right(req) => IO(Handler(req)).flatMap(Ok(_)).handleErrorWith(th =>
              Ok(JsonRpc.error(JsonRpc.ErrorCode.InternalError, th.getMessage))
            )

    val appWithLogging =
      Logger.httpRoutes(logHeaders = false, logBody = true, redactHeadersWhen = _ => false)(app)
    val httpApp = Router("/" -> appWithLogging).orNotFound

    for
      given Logger[IO] <- LoggerFactory[IO].create
      host <- Host
        .fromString(host)
        .liftTo[IO](HttpServerException(s"Invalid host $host"))
      port <- Port
        .fromInt(port)
        .liftTo[IO](HttpServerException(s"Invalid port $port"))
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withHttpApp(httpApp)
        .withMaxConnections(8)
        .build
        .evalTap(_ => info"Server listening on port $port")
        .use(_ => IO.never)
    yield ()
