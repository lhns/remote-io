package de.lolhens.remoteio

import cats.data.OptionT
import cats.effect.kernel.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Functor, Inject}
import de.lolhens.remoteio.Rest.{RestArgs, RestCodec}
import de.lolhens.remoteio.Rpc.{LocalRpcImpl, Protocol, RemoteRpcImpl, RpcRoutes}
import org.http4s.client.Client
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Request, Response, Status, Uri}

trait Rest extends Protocol[Rest] {
  override type Args[F[_], A, B] = RestArgs[A, B, _]

  override type Codec[F[_], A] = RestCodec[F, A]
}

object Rest extends Rest {
  final case class RestArgs[A, B, UriPath](method: Method,
                                           constPrefixSegments: Vector[Uri.Path.Segment],
                                           route: Inject[UriPath, Uri.Path])

  object RestArgs {
    implicit def methodAndPathArgs[A, B](route: (Method, Uri.Path)): RestArgs[A, B, Unit] =
      RestArgs(route._1, route._2.segments, new Inject[Unit, Uri.Path] {
        override val inj: Unit => Uri.Path = _ => Uri.Path(route._2.segments)

        override val prj: Uri.Path => Option[Unit] = _ => Some(())
      })

    implicit def matchPath[UriPath, Body, B](route: (Method, Inject[UriPath, Uri.Path])): RestArgs[PathAndBody[UriPath, Body], B, UriPath] =
      RestArgs(route._1, Vector.empty, route._2)
  }

  implicit def pathAndBodyEntityCodec[F[_] : Functor, UriPath, Body](implicit
                                                                     decoder: EntityDecoder[F, Body],
                                                                     encoder: EntityEncoder[F, Body]): RestCodec[F, PathAndBody[UriPath, Body]] =
    RestCodec(decoder.map(e => PathAndBody(???, e)), encoder.contramap(_.body))

  case class PathAndBody[UriPath, Body](uriPath: UriPath, body: Body)

  case class RestRpcImpl[F[_] : Sync](client: Client[F], uri: Uri) extends RemoteRpcImpl[F, Rest] {
    override def run[A, B, Args](rpc: Rpc[F, A, B, Rest], a: A): F[B] = {
      implicit val encoder: EntityEncoder[F, A] = rpc.aCodec.encoder
      implicit val decoder: EntityDecoder[F, B] = rpc.bCodec.decoder
      client.expect[B](Request[F](
        method = rpc.args.method,
        uri = uri.withPath(uri.path.addSegments(rpc.args.constPrefixSegments))
      ).withEntity(a))
    }
  }

  final case class RestCodec[F[_], A](decoder: EntityDecoder[F, A],
                                      encoder: EntityEncoder[F, A])

  object RestCodec extends RestCodecLowPrioImplicits {
    implicit def unitEntityCodec[F[_] : Concurrent]: RestCodec[F, Unit] =
      RestCodec(EntityDecoder.void, EntityEncoder.unitEncoder)
  }

  trait RestCodecLowPrioImplicits {
    implicit def entityCodec[F[_], A](implicit
                                      decoder: EntityDecoder[F, A],
                                      encoder: EntityEncoder[F, A]): RestCodec[F, A] =
      RestCodec(decoder, encoder)
  }

  implicit class RpcRoutesRestOps[F[_]](routes: RpcRoutes[F, Rest]) {
    def toRoutes(implicit F: Sync[F]): HttpRoutes[F] = {
      val implMap: Map[(Method, Vector[Uri.Path.Segment]), LocalRpcImpl[F, _, _, Rest]] =
        routes.impls.map(impl => (impl.rpc.args.method, impl.rpc.args.constPrefixSegments) -> impl).toMap

      def run[A, B](impl: LocalRpcImpl[F, A, B, Rest], request: Request[F]): F[Response[F]] = {
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
          impl <- OptionT.fromOption[F](implMap.get((request.method, request.pathInfo.segments)))
          response <- OptionT.liftF(run(impl, request))
        } yield
          response
      }
    }
  }
}
