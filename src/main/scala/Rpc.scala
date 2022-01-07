import Rpc.{Protocol, RpcServerImpl}

final case class Rpc[F[_], A, B, P <: Protocol[P]] private(protocol: P,
                                                           id: P#Id)
                                                          (val aCodec: P#Codec[F, A],
                                                           val bCodec: P#Codec[F, B]) {
  def apply(a: A)(implicit impl: protocol.ClientImpl[F]): F[B] = impl.run(this, a)

  def impl(f: A => F[B]): RpcServerImpl[F, A, B, P] = new RpcServerImpl[F, A, B, P](this, f)
}

object Rpc {
  def apply[F[_], A, B, P <: Protocol[P]](id: P#Id)
                                         (implicit
                                          protocol: P,
                                          aCodec: P#Codec[F, A],
                                          bCodec: P#Codec[F, B]): Rpc[F, A, B, P] =
    new Rpc[F, A, B, P](protocol, id)(aCodec, bCodec)

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
