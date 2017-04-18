package io.github.johnpmayer.diff.keyed

import io.github.johnpmayer.diff.keyed.KeyedSeqElementDiff.{Added, Changed, Moved, Removed}
import io.github.johnpmayer.diff.{Diff, Diffable}

import scala.annotation.tailrec

object KeyedSeqDiffable {
  object ChangeBuckets {
    def empty[K, E]: ChangeBuckets[K, E] = ChangeBuckets[K, E](Seq.empty, Seq.empty, Seq.empty)
  }

  case class ChangeBuckets[K, E](added: Seq[(K, Int, E)], removed: Seq[(K, Int, E)], kept: Seq[(K, Int, E, Int, E)]) {
    def add(key: K, index: Int, compareElement: E): ChangeBuckets[K, E] =
      this.copy(added = (key, index, compareElement) +: this.added)

    def remove(key: K, index: Int, baseElement: E): ChangeBuckets[K, E] =
      this.copy(removed = (key, index, baseElement) +: this.removed)

    def keep(key: K, fromIndex: Int, baseElement: E, toIndex: Int, compareElement: E): ChangeBuckets[K, E] =
      this.copy(kept = (key, fromIndex, baseElement, toIndex, compareElement) +: this.kept)
  }

  @tailrec def bucketElements[K <: Ordered[K], E](sortedBase: Seq[(K, Int, E)],
                                                  sortedCompare: Seq[(K, Int, E)],
                                                  acc: ChangeBuckets[K, E]): ChangeBuckets[K, E] = {

    (sortedBase, sortedCompare) match {
      case (Nil, Nil) =>  acc

      case (Nil, (compareKey, index, compareElement) :: remainingCompare) =>
        bucketElements(Nil, remainingCompare, acc.add(compareKey, index, compareElement))

      case ((baseKey, _, _):: _, (compareKey, index, compareElement) :: remainingCompare)
        if compareKey < baseKey =>
        bucketElements(sortedBase, remainingCompare, acc.add(compareKey, index, compareElement))

      case ((baseKey, index, baseElement) :: remainingBase, Nil) =>
        bucketElements(remainingBase, Nil, acc.remove(baseKey, index, baseElement))

      case ((baseKey, index, baseElement) :: remainingBase, (compareKey, _, _) :: _)
        if baseKey < compareKey =>
        bucketElements(remainingBase, sortedCompare, acc.remove(baseKey, index, baseElement))

      case ((baseKey, fromIndex, baseElement) :: remainingBase, (compareKey, toIndex, compareElement) :: remainingCompare)
        if baseKey.compare(compareKey) == 0 =>
        bucketElements(remainingBase, remainingCompare, acc.keep(baseKey, fromIndex, baseElement, toIndex, compareElement))
    }
  }

  def keyedSequenceDiff[K <: Ordered[K], E <: Keyed[K], C <: Seq[E]](base: C,
                                                                     compare: C)
                                                                    (implicit diff: Diffable[E]): Diff[C] = {

    val buckets = bucketElements(
      base.zipWithIndex.map { case (x,i) => (x.getKey, i, x) }.sortBy(_._1),
      compare.zipWithIndex.map { case (x,i) => (x.getKey, i, x) }.sortBy(_._1),
      ChangeBuckets.empty)

    val removed: Seq[(K, KeyedSeqElementDiff[E])] = buckets.removed.map {
      case (removedKey, index, removedElement) => removedKey -> Removed(removedElement, index)
    }

    val added: Seq[(K, KeyedSeqElementDiff[E])] = buckets.added.map {
      case (addedKey, index, addedElement) => addedKey -> Added(addedElement, index)
    }

    val (unmovedPre, movedPre) = buckets.kept.partition {
      case (_, fromIndex, _, toIndex, _) => fromIndex == toIndex
    }

    val changed: Seq[(K, KeyedSeqElementDiff[E])] = unmovedPre map {
      case (keptKey, fromIndex, baseElement, _, compareElement) => (keptKey, fromIndex, diff.diff(baseElement, compareElement))
    } collect {
      case (keptKey, index, elementDiff) if elementDiff.changed => keptKey -> Changed(elementDiff, index)
    }

    val moved: Seq[(K, KeyedSeqElementDiff[E])] = movedPre map {
      case (keptKey, fromIndex, baseElement, toIndex, compareElement) => (keptKey, fromIndex, toIndex, diff.diff(baseElement, compareElement))
    } collect {
      case (keptKey, fromIndex, toIndex, elementDiff) if elementDiff.changed => keptKey -> Moved(elementDiff, fromIndex, toIndex)
    }

    KeyedSeqDiff(removed ++ added ++ changed ++ moved)
  }
}
