package io.github.johnpmayer.diff.keyed

import io.github.johnpmayer.diff.{Diff, DiffStats}

case class KeyedSeqDiff[K <: Ordered[K], E <: Keyed[K], C <: Seq[E]](changes: Seq[(K, KeyedSeqElementDiff[E])]) extends Diff[C] {
  override def stats: DiffStats = ???
  override def changed: Boolean = changes.nonEmpty
}

