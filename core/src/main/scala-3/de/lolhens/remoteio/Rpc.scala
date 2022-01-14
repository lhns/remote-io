package de.lolhens.remoteio

import cats.syntax.invariant._
import cats.syntax.functor._
import cats.{Functor, Invariant}
import de.lolhens.remoteio.Biinvariant.syntax._
import de.lolhens.remoteio.Rpc.{LocalRpcImpl, Protocol, RemoteRpcImpl, RpcRoutes}

sealed trait MappedRpc[F[_], A, B, P <: Protocol[P]] {
  val protocol: P
  def args: protocol.Args[F, A, B]
  def aCodec: protocol.Codec[F, A]
  def bCodec: protocol.Codec[F, B]

  def apply(a: A)(implicit impl: RemoteRpcImpl[F, P]): F[B]
}

sealed abstract case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P)
                                                                    (val args: protocol.Args[F, A, B])
                                                                    (val aCodec: protocol.Codec[F, A],
                                                                     val bCodec: protocol.Codec[F, B]) extends MappedRpc[F, A, B, P] {
  def apply(a: A)(implicit impl: RemoteRpcImpl[F, P]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): LocalRpcImpl[F, A, B, P] = new LocalRpcImpl[F, A, B, P](this, f) {}

  private val eqObj = (protocol, args)

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
                               (args: protocol.Args[F, A, B])
                               (implicit
                                aCodec: protocol.Codec[F, A],
                                bCodec: protocol.Codec[F, B]): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol)(args)(aCodec, bCodec) {}

    def apply[P <: Protocol[P]](protocol: P)
                               ()
                               (implicit
                                args: protocol.Args[F, A, B],
                                aCodec: protocol.Codec[F, A],
                                bCodec: protocol.Codec[F, B],
                                dummyImplicit: DummyImplicit): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol)(args)(aCodec, bCodec) {}
  }

  def apply[F[_], A, B]: RpcPartiallyApplied[F, A, B] = new RpcPartiallyApplied[F, A, B](())

  implicit def biinvariant[F[_]: Functor, P <: Protocol[P]]: Biinvariant[[A, B] =>> MappedRpc[F, A, B, P]] = {
    type RpcFP[A, B] = MappedRpc[F, A, B, P]
    new Biinvariant[RpcFP] {
      override def biimap[A, B, C, D](fab: RpcFP[A, B])(fa: A => C)(ga: C => A)(fb: B => D)(gb: D => B): RpcFP[C, D] = {
        type ArgsF[A, B] = fab.protocol.Args[F, A, B]
        type CodecF[A] = fab.protocol.Codec[F, A]
        implicit val biinvariantArgs: Biinvariant[ArgsF] = fab.protocol.argsBiinvariant[F]
        implicit val invariantCodec: Invariant[CodecF] = fab.protocol.codecInvariant[F]
        new MappedRpc[F, C, D, P] {
          override val protocol: P = fab.protocol
          override def args: protocol.Args[F, C, D] = fab.args.biimap(fa)(ga)(fb)(gb).asInstanceOf[protocol.Args[F, C, D]]
          override def aCodec: protocol.Codec[F, C] = fab.aCodec.imap(fa)(ga).asInstanceOf[protocol.Codec[F, C]]
          override def bCodec: protocol.Codec[F, D] = fab.bCodec.imap(fb)(gb).asInstanceOf[protocol.Codec[F, D]]

          override def apply(a: C)(implicit impl: RemoteRpcImpl[F, P]): F[D] = fab.apply(ga(a)).map(fb)
        }
      }
    }
  }

  type BiinvariantF[F[_], G[_[_], _, _]] = Biinvariant[[A, B] =>> G[F, A, B]]
  type InvariantF[F[_], G[_[_], _]] = Invariant[[A] =>> G[F, A]]

  trait Protocol[P <: Protocol[P]] {
    type Args[F[_], A, B]

    type Codec[F[_], A]

    def argsBiinvariant[F[_]: Functor]: BiinvariantF[F, Args]

    def codecInvariant[F[_]: Functor]: InvariantF[F, Codec]
  }

  trait RemoteRpcImpl[F[_], P <: Protocol[P]] {
    def run[A, B, Args](rpc: Rpc[F, A, B, P], a: A): F[B]
  }

  sealed abstract case class LocalRpcImpl[F[_], A, B, P <: Protocol[P]] private[Rpc](rpc: Rpc[F, A, B, P],
                                                                                     run: A => F[B])

  final case class RpcRoutes[F[_], P <: Protocol[P]](impls: LocalRpcImpl[F, _, _, P]*) {
    impls.groupBy(_.rpc).foreach {
      case (rpc, impls) =>
        if (impls.size > 1)
          throw new IllegalArgumentException(s"rpc must be unique: $rpc")
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
      override def run[A, B, Args](rpc: Rpc[F, A, B, P], a: A): F[B] = apply(rpc).run(a)
    }
  }
}
