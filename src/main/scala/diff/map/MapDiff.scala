package diff.map

import diff.{Diff, DiffStats}

case class MapDiff[K <: Ordered[K], E, C <: Map[K, E]](changes: Seq[(K, MapElementDiff[E])]) extends Diff[C] {
  override def stats: DiffStats = ???
  override def changed: Boolean = changes.nonEmpty
}

