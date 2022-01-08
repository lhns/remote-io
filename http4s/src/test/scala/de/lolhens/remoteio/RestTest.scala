package de.lolhens.remoteio

import cats.effect.IO
import de.lolhens.remoteio.Rest.RestClientImpl
import org.http4s.Method.{GET, POST}
import org.http4s.Uri
import org.http4s.Uri.Path
import org.http4s.client.Client

class RestTest extends CatsEffectSuite {
  object TestRepo {
    val hello = Rpc[IO, Unit, String](Rest)(GET -> Path.empty / "api" / "hello")
    val world = Rpc[IO, String, String](Rest)(POST -> Path.empty / "api" / "world")
  }

  val routes = Rest.toRoutes(
    TestRepo.hello.impl { _ =>
      IO.pure("hello")
    },
    TestRepo.world.impl { string =>
      IO.pure(string + " world")
    }
  )

  test("client and server") {
    implicit val rpcClient: RestClientImpl[IO] = RestClientImpl[IO](
      Client.fromHttpApp(routes.orNotFound),
      Uri.unsafeFromString("http://localhost:8080")
    )

    TestRepo.hello(()).map { e => println(e) } >>
      TestRepo.world("b").map { e => println(e) }
  }
}
