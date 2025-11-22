package frontend

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.std.Queue
import cats.effect.std.SecureRandom
import cats.effect.std.Supervisor
import fs2.dom.Window

import scala.concurrent.duration.*

object Store:

  def resource[F[_]: SecureRandom](using F: Async[F]): Resource[F, ff4s.Store[F, State, Action]] =
    for
      sendQ <- Queue.unbounded[F, dtos.WSProtocol.Client].toResource

      supervisor <- Supervisor[F]

      notifsQ <- Queue.unbounded[F, Notification].toResource

      localStorage <- Resource.pure(Window[F].localStorage)

      store <- ff4s.Store[F, State, Action](State.default): _ =>
        case (Action.SendWS(msg), state)    => state -> sendQ.offer(msg)
        case (Action.ModifyState(f), state) => f(state) -> F.unit

      _ <- F.unit.toResource

      _ <- fs2.Stream
        .fixedDelay(30.second)
        .evalMap(_ => sendQ.offer(dtos.WSProtocol.Client.Ping))
        .compile
        .drain
        .background

      _ <- ff4s.WebSocketClient[F].bidirectionalJson[
        dtos.WSProtocol.Server,
        dtos.WSProtocol.Client
      ](
        s"ws://127.0.0.1:8090/api/ws",
        _.evalMap {
          case dtos.WSProtocol.Server.Pong => F.delay(println("pong received"))
        },
        fs2.Stream.fromQueueUnterminated(sendQ)
      )
        .background
    yield store
