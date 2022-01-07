import Http.{HttpClientImpl, HttpCodec}
import Rpc.{Protocol, RpcClientImpl, RpcServerImpl}
import cats.data.{Kleisli, OptionT}
import cats.effect.kernel.Sync
import cats.syntax.all._
import org.http4s.client.Client
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Request, Response, Status, Uri}

trait Http extends Protocol[Http] {
  override type Id = String

  override type Codec[F[_], A] = HttpCodec[F, A]

  override type ClientImpl[F[_]] = HttpClientImpl[F]
}

object Http {
  class HttpClientImpl[F[_] : Sync](client: Client[F], uri: Uri) extends RpcClientImpl[F, Http] {
    override def run[A, B, Id](rpc: Rpc[F, A, B, Http], a: A): F[B] = {
      implicit val encoder: EntityEncoder[F, A] = rpc.aCodec.encoder
      implicit val decoder: EntityDecoder[F, B] = rpc.bCodec.decoder
      client.expect[B](Request[F](
        method = Method.POST,
        uri = uri / rpc.id
      ).withEntity(a))
    }
  }

  case class HttpCodec[F[_], A](decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, A])

  object HttpCodec {
    implicit def httpCodec[F[_], A](implicit decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, A]): HttpCodec[F, A] =
      HttpCodec(decoder, encoder)
  }

  implicit val protocol: Http = new Http {}

  def toRoutes[F[_] : Sync](impls: RpcServerImpl[F, _, _, Http]*): HttpRoutes[F] = {
    impls.groupBy(_.rpc.id).foreach {
      case (name, impls) =>
        if (impls.size > 1)
          throw new IllegalArgumentException(s"rpc must be registered only once: $name")
    }

    val implMap = impls.map(impl => impl.rpc.id -> impl).toMap

    def run[A, B, Id](impl: RpcServerImpl[F, A, B, Http], request: Request[F]): F[Response[F]] = {
      implicit val decoder: EntityDecoder[F, A] = impl.rpc.aCodec.decoder
      implicit val encoder: EntityEncoder[F, B] = impl.rpc.bCodec.encoder
      request.as[A].flatMap { a =>
        impl.run(a).map { b =>
          Response(Status.Ok).withEntity(b)
        }
      }
    }

    Kleisli { request =>
      for {
        _ <- OptionT.when(request.method == Method.POST)(())
        impl <- OptionT.fromOption[F](implMap.get(request.pathInfo.renderString.replaceFirst("^/", "")))
        response <- OptionT.liftF(run(impl, request))
      } yield
        response
    }
  }
}
