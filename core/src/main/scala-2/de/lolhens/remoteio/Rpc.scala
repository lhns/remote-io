package de.lolhens.remoteio

import de.lolhens.remoteio.Rpc.{Protocol, RpcServerImpl, RpcServerImpls}

sealed abstract case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P,
                                                                     id: P#Id)
                                                                    (val aCodec: P#Codec[F, A],
                                                                     val bCodec: P#Codec[F, B]) {
  def apply(a: A)(implicit impl: protocol.ClientImpl[F]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): RpcServerImpl[F, A, B, P] = new RpcServerImpl[F, A, B, P](this, f) {}

  private var _implCache: (RpcServerImpls[F, P], RpcServerImpl[F, A, B, P]) = null
}

object Rpc {
  final class RpcPartiallyApplied2[F[_], A, B, P <: Protocol[P]] private[Rpc](val protocol: Protocol[P]) extends AnyVal {
    def apply(id: P#Id)
             (implicit
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B]): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol.asInstanceOf[P], id)(aCodec, bCodec) {}

    def apply()
             (implicit
              id: P#Id,
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B],
              dummyImplicit: DummyImplicit): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol.asInstanceOf[P], id)(aCodec, bCodec) {}
  }

  final class RpcPartiallyApplied[F[_], A, B] private[Rpc](val dummy: Unit) extends AnyVal {
    def apply[P <: Protocol[P]](protocol: Protocol[P]): RpcPartiallyApplied2[F, A, B, P] =
      new RpcPartiallyApplied2[F, A, B, P](protocol)
  }

  def apply[F[_], A, B]: RpcPartiallyApplied[F, A, B] = new RpcPartiallyApplied[F, A, B](())

  trait Protocol[P <: Protocol[P]] {
    type Id

    type Codec[F[_], A]

    type ClientImpl[F[_]] <: RpcClientImpl[F, P]
  }

  trait RpcClientImpl[F[_], P <: Protocol[P]] {
    def run[A, B, Id](rpc: Rpc[F, A, B, P], a: A): F[B]
  }

  sealed abstract case class RpcServerImpl[F[_], A, B, P <: Protocol[P]] private[Rpc](rpc: Rpc[F, A, B, P],
                                                                                      run: A => F[B])

  // TODO: think more about this
  final case class RpcServerImpls[F[_], P <: Protocol[P]](impls: Seq[RpcServerImpl[F, _, _, P]]) {
    protected val implMap: Map[P#Id, RpcServerImpl[F, _, _, P]] = impls.map(impl => impl.rpc.id -> impl).toMap

    def apply[A, B](rpc: Rpc[F, A, B, P]): RpcServerImpl[F, A, B, P] = {
      val implCache = rpc._implCache
      if (implCache != null && (implCache._1 eq this)) {
        implCache._2
      } else {
        val impl = implMap(rpc.id).asInstanceOf[RpcServerImpl[F, A, B, P]]
        rpc._implCache = (this, impl)
        impl
      }
    }
  }
}
