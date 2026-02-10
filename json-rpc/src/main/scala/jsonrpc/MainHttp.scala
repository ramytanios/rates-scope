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
import org.http4s.server.middleware.Logger

object MainHttp extends IOApp.Simple:

  case class Settings(
      host: String = "localhost",
      port: Int = 8090,
      maxConnections: Int = 1024
  )

  case class HttpServerException(msg: String) extends RuntimeException(msg)

  override def run: IO[Unit] =

    val settings = Settings()

    val app = HttpRoutes.of[IO]:
      case request @ POST -> Root / "rpc" =>
        request.as[JsonRpc.Request].attempt.flatMap:
          case Left(th) => Ok(JsonRpc.error(JsonRpc.ErrorCode.ParseError, th.getMessage))
          case Right(req) => IO(Handler(req)).flatMap(Ok(_)).handleErrorWith(th =>
              Ok(JsonRpc.error(JsonRpc.ErrorCode.InternalError, th.getMessage, req.id))
            )

    val appWithLogging =
      Logger.httpRoutes(logHeaders = false, logBody = true, redactHeadersWhen = _ => false)(app)
    val httpApp = Router("/" -> appWithLogging).orNotFound

    for
      host <- Host
        .fromString(settings.host)
        .liftTo[IO](HttpServerException(s"Invalid host ${settings.host}"))
      port <- Port
        .fromInt(settings.port)
        .liftTo[IO](HttpServerException(s"Invalid port ${settings.port}"))
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withHttpApp(httpApp)
        .withMaxConnections(settings.maxConnections)
        .build
        .evalTap(_ => IO.println("Server listening on port $port"))
        .use(_ => IO.never)
    yield ()
