package optics
package classic

abstract class Lens[S, T] extends ALens[S, T] {self =>

  def get(s: S): T

  def set(t: T): S => S

  def modify(f: T => T): S => S = {
    (s:S) =>
      val t: T = get(s)
      val appliedF = f(t)
      set(appliedF)(s)
  }

  def composeLens[C](other: Lens[T, C]): Lens[S, C] = new Lens[S, C]{
    override def get(s: S): C = other.get(self.get(s))

    override def set(c: C): S => S = {
      s:S => {
        val sth3: T => T = other.set(c)
        self.modify(sth3)(s)
      }
    }
  }
}

object Lens {
  def apply[S, T](_get: S => T)(_set: T => S => S): Lens[S, T] = new Lens[S, T] {

    def get(s: S): T = _get(s)

    def set(t: T): S => S = s => _set(t)(s)
  }
}
