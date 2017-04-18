package io.github.johnpmayer.diff.map

import io.github.johnpmayer.diff.Diff

trait MapElementDiff[+T]

object MapElementDiff {
  case class Changed[T](diff: Diff[T]) extends MapElementDiff[T]
  case class Added[T](element: T) extends MapElementDiff[T]
  case class Removed[T](element: T) extends MapElementDiff[T]
}
