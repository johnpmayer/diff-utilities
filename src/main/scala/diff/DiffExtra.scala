package diff

import scalaz.Monoid
import scalaz.syntax.semigroup._

// TODO: migrate everything in this file to a better place

trait DataDiff[+T] {
  def changed: Boolean

  def compressUnchanged: DataDiff[T] =
    if (changed) this else DataDiff.Unchanged

  def modifiedCount: ModifiedCount
}

sealed trait SequenceElementDiff[+T] {
  def changed: Boolean
}

trait SequenceDiff[+K, +T] {
  def changed: Boolean

  def compressUnchangedSequence: SequenceDiff[K, T] =
    if (changed) this else DataDiff.Unchanged

  def modifiedCount: ModifiedCount
}

object DataDiff {

  case object Unchanged
      extends DataDiff[Nothing]
      with SequenceDiff[Nothing, Nothing] {
    override def changed: Boolean = ???

    override def modifiedCount: ModifiedCount = ???
  }

  case class ValueChanged[T](value: T) extends DataDiff[T] {
    override def changed: Boolean = true

    override def modifiedCount: ModifiedCount = ModifiedCount.changedOne
  }

}

object SequenceElement {

  case class UnMoved[T](diff: DataDiff[T]) extends SequenceElementDiff[T] {
    override def changed: Boolean = diff.changed
  }

  case class Added[T](to: BigInt, value: T) extends SequenceElementDiff[T] {
    override def changed: Boolean = true
  }

  case class Deleted[T](from: BigInt, value: T)
      extends SequenceElementDiff[T] {
    override def changed: Boolean = true
  }

  case class Moved[T](from: BigInt, to: BigInt, diff: DataDiff[T])
      extends SequenceElementDiff[T] {
    override def changed: Boolean = true
  }

}

case class SequenceChanged[K, T](
    elementChanges: List[(K, SequenceElementDiff[T])])
    extends SequenceDiff[K, T] {
  override def changed: Boolean = elementChanges.exists {
    case (_, element) => element.changed
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case SequenceChanged(otherElementChanges) =>
      elementChanges.toMap.equals(otherElementChanges.toMap)
    case _ => false
  }

  override def modifiedCount: ModifiedCount =
    elementChanges.collect {
      case (_, SequenceElement.UnMoved(diff)) => diff.modifiedCount
      case (_, _: SequenceElement.Added[T]) => ModifiedCount.addedOne
      case (_, _: SequenceElement.Deleted[T]) => ModifiedCount.deletedOne
      case (_, SequenceElement.Moved(_, _, diff)) =>
        diff.modifiedCount |+| ModifiedCount.changedOne
    }.reduceMonoid
}

object DiffExtra {

  def labeledSequenceDiff[K, T](getKey: T => K,
                                dataDiff: (T, T) => DataDiff[T],
                                base: Seq[T],
                                compare: Seq[T]): SequenceDiff[K, T] = {

    val baseLabeled: Map[K, (BigInt, T)] = base.zipWithIndex.map {
      case (x, index) => getKey(x) -> (BigInt(index) -> x)
    }.toMap
    val compareLabeled: Map[K, (BigInt, T)] = compare.zipWithIndex.map {
      case (x, index) => getKey(x) -> (BigInt(index) -> x)
    }.toMap
    val addedKeys: Set[K] = compareLabeled.keySet.diff(baseLabeled.keySet)
    val deletedKeys: Set[K] = baseLabeled.keySet.diff(compareLabeled.keySet)
    val sameKeys: Set[K] = baseLabeled.keySet.intersect(compareLabeled.keySet)

    val added: Set[(K, SequenceElementDiff[T])] =
      addedKeys.map(
        newKey =>
          newKey -> (SequenceElement.Added.apply[T] _)
            .tupled(compareLabeled
              .getOrElse(newKey, sys.error("Diff invariant error"))))

    val deleted: Set[(K, SequenceElementDiff[T])] =
      deletedKeys.map(
        oldKey =>
          oldKey -> (SequenceElement.Deleted.apply[T] _)
            .tupled(baseLabeled.getOrElse(oldKey,
                                          sys.error("Diff invariant error"))))

    val same: Set[(K, SequenceElementDiff[T])] =
      sameKeys.map { key =>
        val (oldIndex, oldVal) =
          baseLabeled.getOrElse(key, sys.error("Diff invariant error"))
        val (newIndex, newVal) =
          compareLabeled.getOrElse(key, sys.error("Diff invariant error"))
        key -> {
          if (oldIndex == newIndex) {
            SequenceElement.UnMoved(dataDiff(oldVal, newVal))
          } else {
            SequenceElement.Moved(oldIndex, newIndex, dataDiff(oldVal, newVal))
          }
        }
      }

    val changes: Set[(K, SequenceElementDiff[T])] = added ++
        deleted ++
        same.filter {
        case (_, sequenceElementDiff) => sequenceElementDiff.changed
      }

    if (changes.isEmpty) {
      DataDiff.Unchanged
    } else {
      SequenceChanged(changes.toList)
    }

  }
}

object ModifiedCount extends Monoid[ModifiedCount] {

  override val zero = ModifiedCount(None, None, None)
  val addedOne = ModifiedCount(added = Some(1), deleted = None, changed = None)
  val deletedOne =
    ModifiedCount(added = None, deleted = Some(1), changed = None)
  val changedOne =
    ModifiedCount(added = None, deleted = None, changed = Some(1))

  private def appendCountPart(f1: Option[BigInt],
                              f2: Option[BigInt]): Option[BigInt] =
    (f1, f2) match {
      case (None, None) => None
      case (None, n: Some[_]) => n
      case (n: Some[_], None) => n
      case (Some(n1), Some(n2)) => Some(n1 + n2)
    }

  override def append(f1: ModifiedCount, f2: => ModifiedCount): ModifiedCount =
    (f1, f2) match {
      case (ModifiedCount(a1, d1, c1), ModifiedCount(a2, d2, c2)) =>
        ModifiedCount(
          added = appendCountPart(a1, a2),
          deleted = appendCountPart(d1, d2),
          changed = appendCountPart(c1, c2)
        )
    }
}

case class ModifiedCount(added: Option[BigInt],
                         deleted: Option[BigInt],
                         changed: Option[BigInt])
