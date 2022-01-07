import cats.effect.IO
import cats.effect.unsafe.IORuntime
import de.lolhens.remoteio.HttpPost.{HttpPostClientImpl, HttpPostRpcRepo}
import de.lolhens.remoteio.{HttpPost, Rpc}
import org.http4s.Uri
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server.Router

object HttpPostTest {
  object TestRepo extends HttpPostRpcRepo("test") {
    val hello = Rpc[IO, String, String, HttpPost]()
    val world = Rpc[IO, String, String, HttpPost]()
  }

  val routes = HttpPost.toRoutes(
    TestRepo.hello.impl { string =>
      IO.pure("hello " + string)
    },
    TestRepo.world.impl { string =>
      IO.pure(string + " world")
    }
  )

  def main(args: Array[String]): Unit = {
    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(Router("api" -> routes).orNotFound)
      .resource
      .use { _ =>
        JdkHttpClient.simple[IO].use { client =>
          implicit val rpcClient = new HttpPostClientImpl[IO](client, Uri.unsafeFromString("http://localhost:8080/api"))

          TestRepo.hello("a").map { e => println(e) } >>
            TestRepo.world("b").map { e => println(e) }
        }
      }.unsafeRunSync()(IORuntime.global)
  }
}
