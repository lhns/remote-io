package de.lolhens.remoteio

import cats.data.{Kleisli, OptionT}
import cats.effect.kernel.Sync
import cats.syntax.all._
import de.lolhens.remoteio.Rest.{RestClientImpl, RestCodec, RestRpcId}
import de.lolhens.remoteio.Rpc.{Protocol, RpcClientImpl, RpcServerImpl}
import org.http4s.client.Client
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Request, Response, Status, Uri}

trait Rest extends Protocol[Rest] {
  override type Id = RestRpcId

  override type Codec[F[_], A] = RestCodec[F, A]

  override type ClientImpl[F[_]] = RestClientImpl[F]
}

object Rest {
  implicit val instance: Rest = new Rest {}

  final case class RestRpcId private(method: Method, segments: Vector[Uri.Path.Segment])

  object RestRpcId {
    implicit def string2id(route: (Method, Uri.Path)): RestRpcId =
      RestRpcId(route._1, route._2.segments)
  }

  class RestClientImpl[F[_] : Sync](client: Client[F], uri: Uri) extends RpcClientImpl[F, Rest] {
    override def run[A, B, Id](rpc: Rpc[F, A, B, Rest], a: A): F[B] = {
      implicit val encoder: EntityEncoder[F, A] = rpc.aCodec.encoder
      implicit val decoder: EntityDecoder[F, B] = rpc.bCodec.decoder
      client.expect[B](Request[F](
        method = rpc.id.method,
        uri = uri.withPath(uri.path.addSegments(rpc.id.segments))
      ).withEntity(a))
    }
  }

  case class RestCodec[F[_], A](decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, A])

  object RestCodec {
    implicit def entityCodec[F[_], A](implicit
                                      decoder: EntityDecoder[F, A],
                                      encoder: EntityEncoder[F, A]): RestCodec[F, A] =
      RestCodec(decoder, encoder)
  }

  def toRoutes[F[_] : Sync](impls: RpcServerImpl[F, _, _, Rest]*): HttpRoutes[F] = {
    impls.groupBy(_.rpc.id).foreach {
      case (id, impls) =>
        if (impls.size > 1)
          throw new IllegalArgumentException(s"rpc id must be unique: ${id.method} ${Uri.Path(id.segments)}")
    }

    val implMap = impls.map(impl => (impl.rpc.id.method, impl.rpc.id.segments) -> impl).toMap

    def run[A, B, Id](impl: RpcServerImpl[F, A, B, Rest], request: Request[F]): F[Response[F]] = {
      implicit val decoder: EntityDecoder[F, A] = impl.rpc.aCodec.decoder
      implicit val encoder: EntityEncoder[F, B] = impl.rpc.bCodec.encoder
      for {
        a <- request.as[A]
        b <- impl.run(a)
      } yield
        Response(Status.Ok).withEntity(b)
    }

    Kleisli { request =>
      for {
        impl <- OptionT.fromOption[F](implMap.get((request.method, request.pathInfo.segments)))
        response <- OptionT.liftF(run(impl, request))
      } yield
        response
    }
  }
}
