import Http.HttpClientImpl
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.http4s.Uri
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server.Router

object Main {
  val hello: Rpc[IO, String, String, Http] = Rpc("hello")
  val world: Rpc[IO, String, String, Http] = Rpc("world")

  val routes = Http.toRoutes(
    hello.impl { string =>
      IO.pure("hello " + string)
    },
    world.impl { string =>
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

          hello("a").map { e => println(e) } >>
            world("b").map { e => println(e) }
        }
      }.unsafeRunSync()(IORuntime.global)
  }
}
