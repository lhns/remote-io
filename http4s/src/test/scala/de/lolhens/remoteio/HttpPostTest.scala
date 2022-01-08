package de.lolhens.remoteio

import cats.effect.IO
import de.lolhens.remoteio.HttpPost.{HttpPostClientImpl, HttpPostRpcRepo}
import org.http4s.Uri
import org.http4s.client.Client

class HttpPostTest extends CatsEffectSuite {
  object TestRepo extends HttpPostRpcRepo("test") {
    val hello = Rpc[IO, String, String](HttpPost)()
    val world = Rpc[IO, String, String](HttpPost)()
  }

  val routes = HttpPost.toRoutes(
    TestRepo.hello.impl { string =>
      IO.pure("hello " + string)
    },
    TestRepo.world.impl { string =>
      IO.pure(string + " world")
    }
  )

  test("server and client") {
    implicit val rpcClient: HttpPostClientImpl[IO] = HttpPostClientImpl[IO](
      Client.fromHttpApp(routes.orNotFound),
      Uri.unsafeFromString("http://localhost:8080")
    )

    TestRepo.hello("a").map { e => println(e) } >>
      TestRepo.world("b").map { e => println(e) }
  }
}
