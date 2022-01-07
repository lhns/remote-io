import cats.effect.IO
import cats.effect.unsafe.IORuntime
import de.lolhens.remoteio.Rest.RestClientImpl
import de.lolhens.remoteio.{Rest, Rpc}
import org.http4s.Method.{GET, POST}
import org.http4s.Uri
import org.http4s.Uri.Path
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server.Router

object RestTest {
  val hello = Rpc[IO, Unit, String, Rest](GET -> Path.empty / "api" / "hello")
  val world = Rpc[IO, String, String, Rest](POST -> Path.empty / "api" / "world")

  val routes = Rest.toRoutes(
    hello.impl { _ =>
      IO.pure("hello")
    },
    world.impl { string =>
      IO.pure(string + " world")
    }
  )

  def main(args: Array[String]): Unit = {
    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(routes.orNotFound)
      .resource
      .use { _ =>
        JdkHttpClient.simple[IO].use { client =>
          implicit val rpcClient = new RestClientImpl[IO](client, Uri.unsafeFromString("http://localhost:8080"))

          hello().map { e => println(e) } >>
            world("b").map { e => println(e) }
        }
      }.unsafeRunSync()(IORuntime.global)
  }
}
