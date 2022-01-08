package de.lolhens.remoteio

import cats.effect.IO
import de.lolhens.remoteio.Rest.RestClientImpl
import org.http4s.Method.{GET, POST}
import org.http4s.Uri
import org.http4s.Uri.Path
import org.http4s.client.Client

class RestTest extends CatsEffectSuite {
  val hello = Rpc[IO, Unit, String](Rest)(GET -> Path.empty / "api" / "hello")
  val world = Rpc[IO, String, String](Rest)(POST -> Path.empty / "api" / "world")

  val routes = Rest.toRoutes(
    hello.impl { _ =>
      IO.pure("hello")
    },
    world.impl { string =>
      IO.pure(string + " world")
    }
  )

  test("client and server") {
    implicit val rpcClient = RestClientImpl[IO](
      Client.fromHttpApp(routes.orNotFound),
      Uri.unsafeFromString("http://localhost:8080")
    )

    hello(()).map { e => println(e) } >>
      world("b").map { e => println(e) }
  }
}
