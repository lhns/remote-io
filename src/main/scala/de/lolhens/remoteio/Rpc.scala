package de.lolhens.remoteio

import de.lolhens.remoteio.Rpc.{Protocol, RpcServerImpl}

final case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P,
                                                           id: P#Id)
                                                          (val aCodec: P#Codec[F, A],
                                                           val bCodec: P#Codec[F, B]) {
  def apply(a: A)(implicit impl: protocol.ClientImpl[F]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): RpcServerImpl[F, A, B, P] = new RpcServerImpl[F, A, B, P](this, f)
}

object Rpc {
  trait RpcPartiallyApplied2[F[_], A, B, P <: Protocol[P]] {
    def apply(id: P#Id)
             (implicit
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B]): Rpc[F, A, B, P]

    def apply()
             (implicit
              id: P#Id,
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B],
              dummyImplicit: DummyImplicit): Rpc[F, A, B, P]
  }

  trait RpcPartiallyApplied[F[_], A, B] {
    def apply[P <: Protocol[P]](protocol: Protocol[P]): RpcPartiallyApplied2[F, A, B, P] = new RpcPartiallyApplied2[F, A, B, P] {
      override def apply(id: P#Id)
                        (implicit
                         aCodec: P#Codec[F, A],
                         bCodec: P#Codec[F, B]): Rpc[F, A, B, P] =
        new Rpc[F, A, B, P](protocol.asInstanceOf[P], id)(aCodec, bCodec)

      override def apply()
                        (implicit
                         id: P#Id,
                         aCodec: P#Codec[F, A],
                         bCodec: P#Codec[F, B],
                         dummyImplicit: DummyImplicit): Rpc[F, A, B, P] =
        new Rpc[F, A, B, P](protocol.asInstanceOf[P], id)(aCodec, bCodec)
    }
  }

  def apply[F[_], A, B]: RpcPartiallyApplied[F, A, B] = new RpcPartiallyApplied[F, A, B] {}

  trait Protocol[P <: Protocol[P]] {
    type Id

    type Codec[F[_], A]

    type ClientImpl[F[_]] <: RpcClientImpl[F, P]
  }

  trait RpcClientImpl[F[_], P <: Protocol[P]] {
    def run[A, B, Id](rpc: Rpc[F, A, B, P], a: A): F[B]
  }

  final case class RpcServerImpl[F[_], A, B, P <: Protocol[P]] private[Rpc](rpc: Rpc[F, A, B, P],
                                                                            run: A => F[B])
}
