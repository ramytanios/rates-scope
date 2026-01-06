package jsonrpc

import cats.effect.IO
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.ember.client.EmberClientBuilder

import scala.io.Source
import scala.util.Using

class JsonRpcSuite extends CatsEffectSuite:

  test("jsonrpc"):

    EmberClientBuilder.default[IO].build.use: httpClient =>
      Uri.fromString("http://localhost:8090/rpc").liftTo[IO].flatMap: uri =>

        val testPriceIO =
          UUIDGen.randomUUID[IO].flatMap: id =>
            parse(Using.resource(Source.fromResource("price.json"))(_.mkString))
              .liftTo[IO].flatMap: params =>
                val rpcRequest = JsonRpc.Request("2.0", "price", params.some, id.toString)
                val body = fs2.Stream.emit(rpcRequest.asJson.noSpaces)
                  .covary[IO].through(fs2.text.utf8.encode)
                val request = Request[IO](Method.POST, uri).withBodyStream(body)
                val isSuccessIO = httpClient.expect[Json](request).map: js =>
                  js.hcursor.get[Option[JsonRpc.Error]]("error").exists(_.isEmpty)
                assertIOBoolean(isSuccessIO)

        val testInvalidMethodIO =
          UUIDGen.randomUUID[IO].flatMap: id =>
            val rpcRequest = JsonRpc.Request("2.0", "foo", JsonObject().toJson.some, id.toString)
            val body = fs2.Stream.emit(rpcRequest.asJson.noSpaces)
              .covary[IO].through(fs2.text.utf8.encode)
            val request = Request[IO](Method.POST, uri).withBodyStream(body)
            val isErrorIO = httpClient.expect[Json](request).map: js =>
              js.hcursor.get[JsonRpc.Error]("error")
                .exists(_.code === JsonRpc.ErrorCode.MethodNotFound.code)
            assertIOBoolean(isErrorIO)

        for
          _ <- testPriceIO
          _ <- testInvalidMethodIO
        yield ()
