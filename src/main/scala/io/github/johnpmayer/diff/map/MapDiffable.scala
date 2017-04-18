package io.github.johnpmayer.diff.map

import io.github.johnpmayer.diff.map.MapElementDiff.{Added, Changed, Removed}
import io.github.johnpmayer.diff.{Diff, Diffable}

import scala.annotation.tailrec

object MapDiffable {
  object ChangeBuckets {
    def empty[K, E]: ChangeBuckets[K, E] = ChangeBuckets[K, E](Seq.empty, Seq.empty, Seq.empty)
  }

  case class ChangeBuckets[K, E](added: Seq[(K, E)], removed: Seq[(K, E)], kept: Seq[(K, E, E)]) {
    def add(key: K, compareElement: E): ChangeBuckets[K, E] =
      this.copy(added = (key -> compareElement) +: this.added)

    def remove(key: K, baseElement: E): ChangeBuckets[K, E] =
      this.copy(removed = (key -> baseElement) +: this.removed)

    def keep(key: K, baseElement: E, compareElement: E): ChangeBuckets[K, E] =
      this.copy(kept = (key, baseElement, compareElement) +: this.kept)
  }

  @tailrec def bucketElements[K <: Ordered[K], E](sortedBase: Seq[(K, E)],
                                                  sortedCompare: Seq[(K, E)],
                                                  acc: ChangeBuckets[K, E]): ChangeBuckets[K, E] = {

    (sortedBase, sortedCompare) match {
      case (Nil, Nil) =>  acc

      case (Nil, (compareKey, compareElement) :: remainingCompare) =>
        bucketElements(Nil, remainingCompare, acc.add(compareKey, compareElement))

      case ((baseKey, _):: _, (compareKey, compareElement) :: remainingCompare)
        if compareKey < baseKey =>
        bucketElements(sortedBase, remainingCompare, acc.add(compareKey, compareElement))

      case ((baseKey, baseElement) :: remainingBase, Nil) =>
        bucketElements(remainingBase, Nil, acc.remove(baseKey, baseElement))

      case ((baseKey, baseElement) :: remainingBase, (compareKey, _) :: _)
        if baseKey < compareKey =>
        bucketElements(remainingBase, sortedCompare, acc.remove(baseKey, baseElement))

      case ((baseKey, baseElement) :: remainingBase, (compareKey, compareElement) :: remainingCompare)
        if baseKey.compare(compareKey) == 0 =>
        bucketElements(remainingBase, remainingCompare, acc.keep(baseKey, baseElement, compareElement))
    }
  }

  def mapDiff[K <: Ordered[K], E, C <: Map[K, E]](base: C,
                                                  compare: C)
                                                 (implicit diff: Diffable[E]): Diff[C] = {

    val buckets: ChangeBuckets[K, E] = bucketElements(base.toSeq.sortBy(_._1), compare.toSeq.sortBy(_._1), ChangeBuckets.empty)

    val removed: Seq[(K, MapElementDiff[E])] = buckets.removed map {
      case (removedKey, removedElement) => removedKey -> Removed(removedElement)
    }

    val added: Seq[(K, MapElementDiff[E])] = buckets.added map {
      case (addedKey, addedElement) => addedKey -> Added(addedElement)
    }

    val changed: Seq[(K, MapElementDiff[E])] = buckets.kept map {
      case (keptKey, baseElement, compareElement) => keptKey -> diff.diff(baseElement, compareElement)
    } collect {
      case (keptKey, elementDiff) if elementDiff.changed => keptKey -> Changed(elementDiff)
    }

    MapDiff(removed ++ added ++ changed)
  }
}