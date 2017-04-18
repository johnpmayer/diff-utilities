package io.github.johnpmayer.diff.keyed

import io.github.johnpmayer.diff.Diff

trait KeyedSeqElementDiff[+T]

object KeyedSeqElementDiff {
  case class Changed[T](diff: Diff[T], index: Int) extends KeyedSeqElementDiff[T]
  case class Added[T](element: T, index: Int) extends KeyedSeqElementDiff[T]
  case class Removed[T](element: T, index: Int) extends KeyedSeqElementDiff[T]
  case class Moved[T](diff: Diff[T], fromIndex: Int, toIndex: Int) extends KeyedSeqElementDiff[T]
}
