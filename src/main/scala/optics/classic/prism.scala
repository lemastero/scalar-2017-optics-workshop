package optics
package classic

abstract class Prism[S, T] extends APrism[S, T] { self =>

  def getOption(s: S): Option[T]

  def reverseGet(t: T): S

  final def modify(f: T => T): S => S =
    s => getOption(s)
      .map(f)
      .map(reverseGet)
      .getOrElse(s)

  final def composePrism[C](other: Prism[T, C]): Prism[S, C] = new Prism[S, C] {
    override def getOption(s: S): Option[C] = self.getOption(s).flatMap(other.getOption)

    override def reverseGet(c: C): S = self.reverseGet(other.reverseGet(c))
  }
}

object Prism {
  def apply[S, T](_getOption: S => Option[T])(_reverseGet: T => S): Prism[S, T] = new Prism[S, T] {

    def getOption(s: S): Option[T] = _getOption(s)

    def reverseGet(t: T): S        = _reverseGet(t)
  }
}
