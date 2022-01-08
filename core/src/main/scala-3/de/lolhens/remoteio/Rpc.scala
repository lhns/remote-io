package de.lolhens.remoteio

import de.lolhens.remoteio.Rpc.{Protocol, RpcServerImpl}

final case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P)
                                                          (val id: protocol.Id)
                                                          (val aCodec: protocol.Codec[F, A],
                                                           val bCodec: protocol.Codec[F, B]) {
  def apply(a: A)(implicit impl: protocol.ClientImpl[F]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): RpcServerImpl[F, A, B, P] = new RpcServerImpl[F, A, B, P](this, f) {}

  private val eqObj = (protocol, id)

  override def equals(obj: Any): Boolean = obj match {
    case rpc: Rpc[F, A, B, P] => rpc.eqObj == eqObj
    case _ => false
  }

  override def hashCode(): Int = eqObj.hashCode()
}

object Rpc {
  final class RpcPartiallyApplied[F[_], A, B] private[Rpc](val unit: Unit) extends AnyVal {
    def apply[P <: Protocol[P]](protocol: P)
                               (id: protocol.Id)
                               (implicit
                                aCodec: protocol.Codec[F, A],
                                bCodec: protocol.Codec[F, B]): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol)(id)(aCodec, bCodec)

    def apply[P <: Protocol[P]](protocol: P)
                               ()
                               (implicit
                                id: protocol.Id,
                                aCodec: protocol.Codec[F, A],
                                bCodec: protocol.Codec[F, B],
                                dummyImplicit: DummyImplicit): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol)(id)(aCodec, bCodec)
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
}
