package io.github.johnpmayer.diff.keyed

trait Keyed[K] {
  def getKey: K
}
