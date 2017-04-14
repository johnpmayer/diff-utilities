package diff.map

import diff.Diff

trait ElementDiff[+T]

object ElementDiff {
//  case object Unchanged extends ElementDiff[Nothing]
  case class Changed[T](diff: Diff[T]) extends ElementDiff[T]
  case class Added[T](element: T) extends ElementDiff[T]
  case class Removed[T](element: T) extends ElementDiff[T]
}
