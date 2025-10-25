package lib

trait EitherSyntax:
  self: munit.FunSuite =>

  extension [V](e: Either[Error, V])
    def failOrAssert(test: V => Unit) =
      e match
        case Left(err) => fail(err.getMessage)
        case Right(v)  => test(v)
