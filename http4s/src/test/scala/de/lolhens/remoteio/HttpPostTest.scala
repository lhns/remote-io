package de.lolhens.remoteio

import cats.effect.IO
import de.lolhens.remoteio.HttpPost.{HttpPostRpcImpl, HttpPostRpcRepo}
import de.lolhens.remoteio.Rpc.{RemoteRpcImpl, RpcRoutes}
import org.http4s.Uri
import org.http4s.client.Client

class HttpPostTest extends CatsEffectSuite {
  object TestRepo extends HttpPostRpcRepo("test") {
    val rpc1: Rpc[IO, String, String, HttpPost] = Rpc[IO, String, String](HttpPost)()
    val rpc2: Rpc[IO, String, String, HttpPost] = Rpc[IO, String, String](HttpPost)()
  }

  val routes = RpcRoutes(
    TestRepo.rpc1.impl { string =>
      IO.pure("rpc1: " + string)
    },
    TestRepo.rpc2.impl { string =>
      IO.pure("rpc2: " + string)
    }
  ).toRoutes

  test("server and client") {
    implicit val rpcClient: RemoteRpcImpl[IO, HttpPost] = HttpPostRpcImpl[IO](
      Client.fromHttpApp(routes.orNotFound),
      Uri.unsafeFromString("http://localhost:8080")
    )

    TestRepo.rpc1("a").map { e => assertEquals(e, "rpc1: a") } >>
      TestRepo.rpc2("b").map { e => assertEquals(e, "rpc2: b") }
  }
}
