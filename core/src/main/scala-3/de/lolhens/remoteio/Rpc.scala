package de.lolhens.remoteio

import de.lolhens.remoteio.Rpc.{LocalRpcImpl, Protocol, RemoteRpcImpl, RpcRoutes}

final case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P)
                                                          (val id: protocol.Id)
                                                          (val aCodec: protocol.Codec[F, A],
                                                           val bCodec: protocol.Codec[F, B]) {
  def apply(a: A)(implicit impl: RemoteRpcImpl[F, P]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): LocalRpcImpl[F, A, B, P] = new LocalRpcImpl[F, A, B, P](this, f) {}

  private val eqObj = (protocol, id)

  override def equals(obj: Any): Boolean = obj match {
    case rpc: Rpc[F, A, B, P]@unchecked => rpc.eqObj == eqObj
    case _ => false
  }

  override def hashCode(): Int = eqObj.hashCode()

  private var _implCache: (RpcRoutes[F, P], LocalRpcImpl[F, A, B, P]) = null
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
  }

  trait RemoteRpcImpl[F[_], P <: Protocol[P]] {
    def run[A, B, Id](rpc: Rpc[F, A, B, P], a: A): F[B]
  }

  sealed abstract case class LocalRpcImpl[F[_], A, B, P <: Protocol[P]] private[Rpc](rpc: Rpc[F, A, B, P],
                                                                                     run: A => F[B])

  final case class RpcRoutes[F[_], P <: Protocol[P]](impls: LocalRpcImpl[F, _, _, P]*) {
    impls.groupBy(_.rpc.id).foreach {
      case (id, impls) =>
        if (impls.size > 1)
          throw new IllegalArgumentException(s"rpc id must be unique: $id")
    }

    protected val implMap: Map[Rpc[F, _, _, P], LocalRpcImpl[F, _, _, P]] = impls.map(impl => impl.rpc -> impl).toMap

    def apply[A, B](rpc: Rpc[F, A, B, P]): LocalRpcImpl[F, A, B, P] = {
      val implCache = rpc._implCache
      if (implCache != null && (implCache._1 eq this)) {
        implCache._2
      } else {
        val impl = implMap(rpc).asInstanceOf[LocalRpcImpl[F, A, B, P]]
        rpc._implCache = (this, impl)
        impl
      }
    }

    lazy val localImpl: RemoteRpcImpl[F, P] = new RemoteRpcImpl[F, P] {
      override def run[A, B, Id](rpc: Rpc[F, A, B, P], a: A): F[B] = apply(rpc).run(a)
    }
  }
}
