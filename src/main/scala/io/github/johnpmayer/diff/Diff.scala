package io.github.johnpmayer.diff

trait Diff[T] {
  def stats: DiffStats
  def changed: Boolean
}