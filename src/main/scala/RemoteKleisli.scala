import RemoteKleisli.Protocol
import RemoteKleisli.Protocol.HttpProtocolImpl
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.kernel.Sync
import org.http4s.client.Client
import org.http4s.{EntityDecoder, EntityEncoder, Method, Request, Uri}

final case class RemoteKleisli[F[_], A, B, Id, P <: Protocol[P]] private(protocol: P, id: Id)
                                                                        (implicit
                                                                         val idCodec: P#IdCodec[Id],
                                                                         val aCodec: P#Codec[F, A],
                                                                         val bCodec: P#Codec[F, B]) {
  def kleisli(implicit impl: protocol.Impl[F]): Kleisli[F, A, B] = impl.run(this)

  def apply(a: A)(implicit impl: protocol.Impl[F]): F[B] = kleisli(impl)(a)

  def impl(f: A => F[B]): Kleisli[F, A, B] = ???
}

object RemoteKleisli {
  trait RemoteKleisliPartiallyApplied[F[_], A, B] {
    def apply[Id, P <: Protocol[P]](protocol: P, id: Id)
                                   (implicit
                                    idCodec: P#IdCodec[Id],
                                    aCodec: P#Codec[F, A],
                                    bCodec: P#Codec[F, B]): RemoteKleisli[F, A, B, Id, P]
  }

  def apply[F[_], A, B]: RemoteKleisliPartiallyApplied[F, A, B] = new RemoteKleisliPartiallyApplied[F, A, B] {
    override def apply[Id, P <: Protocol[P]](protocol: P, id: Id)
                                            (implicit idCodec: P#IdCodec[Id],
                                             aCodec: P#Codec[F, A],
                                             bCodec: P#Codec[F, B]): RemoteKleisli[F, A, B, Id, P] =
      RemoteKleisli[F, A, B, Id, P](protocol, id)(idCodec, aCodec, bCodec)
  }

  final case class RemoteKleisliImpl[F[_], A, B, Id, P <: Protocol[P]](remoteKleisli: RemoteKleisli[F, A, B, Id, P], run: A => F[B])

  trait RemoteKleisliImplRouter[F[_]] {

  }

  object RemoteKleisliImplRouter {
    def apply[F[_]](impls: RemoteKleisliImpl[F, _, _, _, _]*): RemoteKleisliImplRouter[F] = new RemoteKleisliImplRouter[F] {

    }
  }

  val test = RemoteKleisli[IO, String, String].apply(Protocol.Http: Protocol.Http, "test")

  test.impl { string =>
    IO.pure("hello")
  }

  implicit val httpProtocolImpl = new HttpProtocolImpl[IO](Client.fromHttpApp(null))

  test("hello")

  trait Protocol[P <: Protocol[P]] {
    type IdCodec[Id]

    type Codec[F[_], A]

    type Impl[F[_]] <: Protocol.Impl[F, P]
  }

  object Protocol {
    trait Impl[F[_], P <: Protocol[P]] {
      def run[A, B, Id](remoteKleisli: RemoteKleisli[F, A, B, Id, P]): Kleisli[F, A, B]
    }

    trait HttpIdCodec[A] {
      def toString(a: A): String
    }

    object HttpIdCodec {
      implicit val stringI: HttpIdCodec[String] = new HttpIdCodec[String] {
        override def toString(a: String): String = a
      }
    }

    trait Http extends Protocol[Http] {
      override type IdCodec[Id] = HttpIdCodec[Id]

      override type Impl[F[_]] = HttpProtocolImpl[F]

      override type Codec[F[_], A] = HttpCodec[F, A]
    }

    object Http extends Http

    class HttpProtocolImpl[F[_] : Sync](client: Client[F]) extends Impl[F, Http] {
      override def run[A, B, Id](remoteKleisli: RemoteKleisli[F, A, B, Id, Http]): Kleisli[F, A, B] =
        Kleisli { a =>
          val uri = Uri.unsafeFromString("http://localhost:8080/api") / remoteKleisli.idCodec.toString(remoteKleisli.id)
          val request = Request[F](method = Method.POST, uri = uri).withEntity(a)(remoteKleisli.aCodec.encoder)
          client.run(request).use { b =>
            implicit val decoder: EntityDecoder[F, B] = remoteKleisli.bCodec.decoder
            b.as[B]
          }
        }
    }
  }

  case class HttpCodec[F[_], A](decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, A])

  object HttpCodec {
    implicit def httpCodec[F[_], A](implicit decoder: EntityDecoder[F, A], encoder: EntityEncoder[F, A]): HttpCodec[F, A] =
      HttpCodec(decoder, encoder)
  }
}
