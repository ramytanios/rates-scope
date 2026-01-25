package jsonrpc

import cats.effect.IO
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import munit.CatsEffectSuite

import scala.io.Source
import scala.util.Using

class JsonRpcSuite extends CatsEffectSuite:

  def testMethodIsSuccess(file: String, method: String): IO[Unit] =
    UUIDGen.randomUUID[IO].flatMap: id =>
      IO.blocking(parse(Using.resource(Source.fromResource(file))(_.mkString)))
        .flatMap(_.liftTo[IO])
        .flatMap: params =>
          val rpcRequest = JsonRpc.Request("2.0", method, params.some, id.toString)
          val isSuccessIO = IO(Handler(rpcRequest).asJson).map: js =>
            js.hcursor.get[Option[JsonRpc.Error]]("error").exists(_.isEmpty)
          assertIOBoolean(isSuccessIO)

  test("jsonrpc"):

    val testPriceIO = testMethodIsSuccess("price.json", "price")

    val testSamplingIO = testMethodIsSuccess("sampling.json", "volsampling")

    val testArbitrageIO = testMethodIsSuccess("arbitrage.json", "arbitrage")

    val testInvalidMethodIO =
      UUIDGen.randomUUID[IO].flatMap: id =>
        val rpcRequest = JsonRpc.Request("2.0", "foo", JsonObject().toJson.some, id.toString)
        val isErrorIO = IO(Handler(rpcRequest).asJson).map: js =>
          js.hcursor.get[JsonRpc.Error]("error")
            .exists(_.code === JsonRpc.ErrorCode.MethodNotFound.code)
        assertIOBoolean(isErrorIO)

    for
    // _ <- testPriceIO
    // _ <- testSamplingIO
    _ <- testArbitrageIO
    // _ <- testInvalidMethodIO
    yield ()
