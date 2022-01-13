package de.lolhens.remoteio

import cats.effect.IO
import de.lolhens.remoteio.Rest.RestRpcImpl
import de.lolhens.remoteio.Rpc.{RemoteRpcImpl, RpcRoutes}
import org.http4s.Method.{GET, POST}
import org.http4s.Uri
import org.http4s.Uri.Path
import org.http4s.client.Client

class RestTest extends CatsEffectSuite {
  object TestRepo {
    val rpc1: Rpc[IO, Unit, String, Rest] = Rpc[IO, Unit, String](Rest)(GET -> Path.empty / "api" / "rpc1")
    val rpc2: Rpc[IO, String, String, Rest] = Rpc[IO, String, String](Rest)(POST -> Path.empty / "api" / "rpc2")
  }

  val rpcRoutes = RpcRoutes(
    TestRepo.rpc1.impl { _ =>
      IO.pure("rpc1")
    },
    TestRepo.rpc2.impl { string =>
      IO.pure("rpc2: " + string)
    }
  )

  test("client and server") {
    implicit val remoteRpcImpl: RemoteRpcImpl[IO, Rest] = RestRpcImpl[IO](
      Client.fromHttpApp(rpcRoutes.toRoutes.orNotFound),
      Uri.unsafeFromString("http://localhost:8080")
    )

    TestRepo.rpc1(()).map { e => assertEquals(e, "rpc1") } >>
      TestRepo.rpc2("b").map { e => assertEquals(e, "rpc2: b") }
  }

  test("local impl") {
    implicit val remoteRpcImpl: RemoteRpcImpl[IO, Rest] = rpcRoutes.localImpl

    TestRepo.rpc1(()).map { e => assertEquals(e, "rpc1") } >>
      TestRepo.rpc2("b").map { e => assertEquals(e, "rpc2: b") }
  }
}
