package io.github.johnpmayer.diff

trait Diffable[T] {
  def diff(base: T, compare: T): Diff[T]
}