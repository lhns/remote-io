package de.lolhens.remoteio

import cats.syntax.invariant._
import cats.{Functor, Invariant}
import de.lolhens.remoteio.Biinvariant.syntax._
import de.lolhens.remoteio.Rpc.{LocalRpcImpl, Protocol, RemoteRpcImpl, RpcRoutes}

sealed abstract case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P,
                                                                     args: P#Args[F, A, B])
                                                                    (val aCodec: P#Codec[F, A],
                                                                     val bCodec: P#Codec[F, B]) {
  def apply(a: A)(implicit impl: RemoteRpcImpl[F, P]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): LocalRpcImpl[F, A, B, P] = new LocalRpcImpl[F, A, B, P](this, f) {}

  private var _implCache: (RpcRoutes[F, P], LocalRpcImpl[F, A, B, P]) = null
}

object Rpc {
  final class RpcPartiallyApplied2[F[_], A, B, P <: Protocol[P]] private[Rpc](val protocol: Protocol[P]) extends AnyVal {
    def apply(args: P#Args[F, A, B])
             (implicit
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B]): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol.asInstanceOf[P], args)(aCodec, bCodec) {}

    def apply()
             (implicit
              args: P#Args[F, A, B],
              aCodec: P#Codec[F, A],
              bCodec: P#Codec[F, B],
              dummyImplicit: DummyImplicit): Rpc[F, A, B, P] =
      new Rpc[F, A, B, P](protocol.asInstanceOf[P], args)(aCodec, bCodec) {}
  }

  final class RpcPartiallyApplied[F[_], A, B] private[Rpc](val dummy: Unit) extends AnyVal {
    def apply[P <: Protocol[P]](protocol: Protocol[P]): RpcPartiallyApplied2[F, A, B, P] =
      new RpcPartiallyApplied2[F, A, B, P](protocol)
  }

  def apply[F[_], A, B]: RpcPartiallyApplied[F, A, B] = new RpcPartiallyApplied[F, A, B](())

  implicit def biinvariant[F[_]: Functor, P <: Protocol[P]]: Biinvariant[({ type L[A, B] = Rpc[F, A, B, P]})#L] = {
    type RpcFP[A, B] = Rpc[F, A, B, P]
    new Biinvariant[RpcFP] {
      override def biimap[A, B, C, D](fab: RpcFP[A, B])(fa: A => C)(ga: C => A)(fb: B => D)(gb: D => B): RpcFP[C, D] = {
        type ArgsF[A, B] = P#Args[F, A, B]
        type CodecF[A] = P#Codec[F, A]
        implicit val biinvariantArgs: Biinvariant[ArgsF] = fab.protocol.argsBiinvariant[F].asInstanceOf[Biinvariant[ArgsF]]
        implicit val invariantCodec: Invariant[CodecF] = fab.protocol.codecInvariant[F].asInstanceOf[Invariant[CodecF]]
        new Rpc[F, C, D, P](
          fab.protocol,
          fab.args.asInstanceOf[ArgsF[A, B]].biimap(fa)(ga)(fb)(gb))(
          fab.aCodec.asInstanceOf[CodecF[A]].imap(fa)(ga),
          fab.bCodec.asInstanceOf[CodecF[B]].imap(fb)(gb)) {}
      }
    }
  }

  type BiinvariantF[F[_], G[_[_], _, _]] = Biinvariant[({ type L[A, B] = G[F, A, B]})#L]
  type InvariantF[F[_], G[_[_], _]] = Invariant[({ type L[A] = G[F, A]})#L]

  trait Protocol[P <: Protocol[P]] {
    type Args[F[_], A, B]

    type Codec[F[_], A]

    def argsBiinvariant[F[_]: Functor]: Biinvariant[({type L[A, B] = Args[F, A, B]})#L]

    def codecInvariant[F[_]: Functor]: Invariant[({type L[A] = Codec[F, A]})#L]
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
