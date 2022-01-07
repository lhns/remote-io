import Http.{HttpClientImpl, HttpRpcRepo}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.http4s.Uri
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server.Router

object Main {
  object TestRepo extends HttpRpcRepo("test") {
    val hello = Rpc[IO, String, String, Http]()
    val world = Rpc[IO, String, String, Http]()
  }

  val routes = Http.toRoutes(
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
          implicit val httpProtocolImpl = new HttpClientImpl[IO](client, Uri.unsafeFromString("http://localhost:8080/api"))

          TestRepo.hello("a").map { e => println(e) } >>
            TestRepo.world("b").map { e => println(e) }
        }
      }.unsafeRunSync()(IORuntime.global)
  }
}
