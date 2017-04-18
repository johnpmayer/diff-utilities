
package io.github.johnpmayer

import io.github.johnpmayer.diff.keyed.{Keyed, KeyedSeqDiffable}
import io.github.johnpmayer.diff.map.MapDiffable

import scalaz.Monoid
import scalaz.syntax.semigroup._

package object diff {

  implicit class ReduceMonoid[T](elems: Seq[T])(implicit impl: Monoid[T]) {
    def reduce: T = elems.fold(impl.zero) {
      case (acc, elem) => acc |+| elem
    }
  }

  implicit val diffStats: DiffStats.type = DiffStats

  implicit class ExtendMap[K <: Ordered[K], E](map: Map[K, E])(implicit val diff: Diffable[E]) extends Diffable[Map[K, E]] {
    override def diff(base: Map[K, E], compare: Map[K, E]): Diff[Map[K, E]] =
      MapDiffable.mapDiff[K, E, Map[K, E]](base, compare)
  }

  implicit class ExtendSeq[K <: Ordered[K], E <: Keyed[K]](seq: Seq[E])(implicit val diff: Diffable[E]) extends Diffable[Seq[E]] {
    override def diff(base: Seq[E], compare: Seq[E]): Diff[Seq[E]] =
      KeyedSeqDiffable.keyedSequenceDiff[K, E, Seq[E]](base, compare)
  }

}
