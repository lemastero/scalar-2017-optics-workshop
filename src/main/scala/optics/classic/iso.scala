package optics
package classic

abstract class Iso[S, T] extends AIso[S, T] { self =>

  def get(s: S): T

  def reverseGet(t: T): S

  def reverse: Iso[T, S] = new Iso[T, S]{

    override def get(t: T): S = self.reverseGet(t)

    override def reverseGet(s: S): T = self.get(s)
  }

  final def modify(f: T => T): S => S =
    s => reverseGet(f(get(s)))

  final def composeIso[C](other: Iso[T, C]): Iso[S, C] = new Iso[S, C] {
    override def get(s: S): C = other.get(self.get(s))

    override def reverseGet(c: C): S = self.reverseGet(other.reverseGet(c))
  }

}

object Iso {
  def apply[S, T](_get: S => T)(_reverseGet: T => S): Iso[S, T] =
    new Iso[S, T] { st =>
      def get(s: S): T        = _get(s)
      def reverseGet(t: T): S = _reverseGet(t)
    }
}