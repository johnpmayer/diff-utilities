import scalaz.Monoid
import scalaz.syntax.semigroup._

package object diff {

  implicit class ReduceMonoid[T](elems: Seq[T])(implicit impl: Monoid[T]) {
    def reduceMonoid: T = elems.fold(impl.zero) {
      case (acc, elem) => acc |+| elem
    }
  }

  implicit val modifiedCount: ModifiedCount.type = ModifiedCount

}
