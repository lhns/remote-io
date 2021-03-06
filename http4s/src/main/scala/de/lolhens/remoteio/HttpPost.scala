package de.lolhens.remoteio

import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.kernel.Sync
import cats.syntax.all._
import de.lolhens.remoteio.HttpPost.{HttpPostCodec, HttpPostRpcId}
import de.lolhens.remoteio.Rpc.{LocalRpcImpl, Protocol, RemoteRpcImpl, RpcRoutes}
import org.http4s.client.Client
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Request, Response, Status, Uri}

trait HttpPost extends Protocol[HttpPost] {
  override type Id = HttpPostRpcId

  override type Codec[F[_], A] = HttpPostCodec[F, A]
}

object HttpPost extends HttpPost {
  final case class HttpPostRpcRepoId(id: String)

  abstract class HttpPostRpcRepo(id: String) {
    protected implicit def repoId: HttpPostRpcRepoId = HttpPostRpcRepoId(id)
  }

  final case class HttpPostRpcId(repoId: HttpPostRpcRepoId, id: String)

  object HttpPostRpcId {
    implicit def auto(implicit repoId: HttpPostRpcRepoId, name: sourcecode.Name): HttpPostRpcId =
      HttpPostRpcId(repoId, name.value)

    implicit def string2id(id: String)(implicit repoId: HttpPostRpcRepoId): HttpPostRpcId =
      HttpPostRpcId(repoId, id)
  }

  case class HttpPostRpcImpl[F[_] : Sync](client: Client[F], uri: Uri) extends RemoteRpcImpl[F, HttpPost] {
    override def run[A, B, Id](rpc: Rpc[F, A, B, HttpPost], a: A): F[B] = {
      implicit val encoder: EntityEncoder[F, A] = rpc.aCodec.encoder
      implicit val decoder: EntityDecoder[F, B] = rpc.bCodec.decoder
      client.expect[B](Request[F](
        method = Method.POST,
        uri = uri / rpc.id.repoId.id / rpc.id.id
      ).withEntity(a))
    }
  }

  final case class HttpPostCodec[F[_], A](decoder: EntityDecoder[F, A],
                                          encoder: EntityEncoder[F, A])

  object HttpPostCodec extends HttpPostCodecLowPrioImplicits {
    implicit def unitEntityCodec[F[_] : Concurrent]: HttpPostCodec[F, Unit] =
      HttpPostCodec(EntityDecoder.void, EntityEncoder.unitEncoder)
  }

  trait HttpPostCodecLowPrioImplicits {
    implicit def entityCodec[F[_], A](implicit
                                      decoder: EntityDecoder[F, A],
                                      encoder: EntityEncoder[F, A]): HttpPostCodec[F, A] =
      HttpPostCodec(decoder, encoder)
  }

  implicit class RpcRoutesHttpPostOps[F[_]](routes: RpcRoutes[F, HttpPost]) {
    def toRoutes(implicit F: Sync[F]): HttpRoutes[F] = {
      def path(id: HttpPostRpcId): Uri.Path = Uri.Path.empty / id.repoId.id / id.id

      val implMap: Map[Vector[Uri.Path.Segment], LocalRpcImpl[F, _, _, HttpPost]] =
        routes.impls.map(impl => path(impl.rpc.id).segments -> impl).toMap

      def run[A, B, Id](impl: LocalRpcImpl[F, A, B, HttpPost], request: Request[F]): F[Response[F]] = {
        implicit val decoder: EntityDecoder[F, A] = impl.rpc.aCodec.decoder
        implicit val encoder: EntityEncoder[F, B] = impl.rpc.bCodec.encoder
        for {
          a <- request.as[A]
          b <- impl.run(a)
        } yield
          Response(Status.Ok).withEntity(b)
      }

      HttpRoutes[F] { request =>
        for {
          _ <- OptionT.when(request.method == Method.POST)(())
          impl <- OptionT.fromOption[F](implMap.get(request.pathInfo.segments))
          response <- OptionT.liftF(run(impl, request))
        } yield
          response
      }
    }
  }
}
