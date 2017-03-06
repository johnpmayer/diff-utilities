package diff

import scalaz.Monoid
import scalaz.syntax.semigroup._

trait DataDiff[+T] extends CollectionElementDiff[T] {
  override def map[U](f: T => U): DataDiff[U] = {
    this match {
      case DataDiff.Unchanged => DataDiff.Unchanged
      case DataDiff.ValueChanged(x) => DataDiff.ValueChanged(f(x))
    }
  }

  def compressUnchanged: DataDiff[T] =
    if (changed) this else DataDiff.Unchanged

  def modifiedCount: ModifiedCount
}

sealed trait CollectionElementDiff[+T] {
  def map[U](f: T => U): CollectionElementDiff[U] = {
    this match {
      case CollectionElement.Deleted(x) => CollectionElement.Deleted(f(x))
      case CollectionElement.Added(x) => CollectionElement.Added(f(x))
      case d: DataDiff[T] => d.map(f)
    }
  }

  def changed: Boolean

  def compressUnchangedElement: CollectionElementDiff[T] =
    if (changed) this else DataDiff.Unchanged
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

trait CollectionDiff[+K, +T] {
  def mapValues[U](f: T => U): CollectionDiff[K, U] = {
    this match {
      case DataDiff.Unchanged => DataDiff.Unchanged
      case SetChanged(elementChanges) =>
        SetChanged(elementChanges.map { case (k, v) => k -> v.map(f) })
    }
  }

  def changed: Boolean

  def compressUnchangedCollection: CollectionDiff[K, T] =
    if (changed) this else DataDiff.Unchanged

  def modifiedCount: ModifiedCount
}

object DataDiff {

  case object Unchanged
      extends DataDiff[Nothing]
      with SequenceDiff[Nothing, Nothing]
      with CollectionDiff[Nothing, Nothing] {
    override def changed: Boolean = false

    override def modifiedCount: ModifiedCount = ModifiedCount.zero
  }

  case class ValueChanged[T](value: T) extends DataDiff[T] {
    override def changed: Boolean = true

    override def modifiedCount: ModifiedCount = ModifiedCount.changedOne
  }

}

object CollectionElement {

  case class Deleted[T](value: T) extends CollectionElementDiff[T] {
    override def changed: Boolean = true
  }

  case class Added[T](value: T) extends CollectionElementDiff[T] {
    override def changed: Boolean = true
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

case class SetChanged[K, T](
    elementChanges: List[(K, CollectionElementDiff[T])])
    extends CollectionDiff[K, T] {
  override def changed: Boolean = elementChanges.exists {
    case (_, element) => element.changed
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case SetChanged(otherElementChanges) =>
      elementChanges.toMap.equals(otherElementChanges.toMap)
    case _ => false
  }

  override def modifiedCount: ModifiedCount =
    elementChanges.collect {
      case (_, diff: DataDiff[T]) => diff.modifiedCount
      case (_, _: CollectionElement.Added[T]) => ModifiedCount.addedOne
      case (_, _: CollectionElement.Deleted[T]) => ModifiedCount.deletedOne
    }.reduceMonoid
}

case class OptionChanged[T](elementChange: CollectionElementDiff[T])
    extends CollectionDiff[Unit, T] {
  override def changed: Boolean = elementChange.changed

  override def modifiedCount: ModifiedCount =
    if (elementChange.changed) ModifiedCount.changedOne else ModifiedCount.zero
}

trait Diffable[T] {
  protected def diffUncompressed(base: T, compare: T): DataDiff[T]

  def diff(base: T, compare: T): DataDiff[T] =
    diffUncompressed(base, compare).compressUnchanged
}

object Diffable {

  def valueDiff[T](base: T, compare: T): DataDiff[T] = {
    if (base == compare) DataDiff.Unchanged else DataDiff.ValueChanged(compare)
  }

  def optionDiff[T](base: Option[T],
                    compare: Option[T]): CollectionDiff[Unit, T] = {
    (base, compare) match {
      case (None, None) => DataDiff.Unchanged
      case (None, Some(x)) => OptionChanged(CollectionElement.Added(x))
      case (Some(x), None) => OptionChanged(CollectionElement.Deleted(x))
      case (Some(x), Some(y)) =>
        OptionChanged(valueDiff(x, y)).compressUnchangedCollection
    }
  }

  def optionDiffWith[T](dataDiff: (T, T) => DataDiff[T],
                        base: Option[T],
                        compare: Option[T]): CollectionDiff[Unit, T] = {
    (base, compare) match {
      case (None, None) => DataDiff.Unchanged
      case (None, Some(x)) => OptionChanged(CollectionElement.Added(x))
      case (Some(x), None) => OptionChanged(CollectionElement.Deleted(x))
      case (Some(x), Some(y)) =>
        OptionChanged(dataDiff(x, y)).compressUnchangedCollection
    }
  }

  def collectionDiff[K, T](getKey: T => K,
                           dataDiff: (T, T) => DataDiff[T],
                           base: Iterable[T],
                           compare: Iterable[T]): CollectionDiff[K, T] = {

    val baseLabeled: Map[K, T] = base.map(x => getKey(x) -> x).toMap
    val compareLabeled: Map[K, T] = compare.map(x => getKey(x) -> x).toMap

    mapDiff(dataDiff, baseLabeled, compareLabeled)
  }

  def mapDiff[K, T](dataDiff: (T, T) => DataDiff[T],
                    base: Map[K, T],
                    compare: Map[K, T]): CollectionDiff[K, T] = {

    val addedKeys: Set[K] = compare.keySet.diff(base.keySet)
    val deletedKeys: Set[K] = base.keySet.diff(compare.keySet)
    val sameKeys: Set[K] = base.keySet.intersect(compare.keySet)

    val changes: Set[(K, CollectionElementDiff[T])] =
      addedKeys.map(
        newKey =>
          newKey -> CollectionElement.Added(
            compare.getOrElse(newKey, sys.error("Diff invariant error")))) ++
        deletedKeys.map(
          oldKey =>
            oldKey -> CollectionElement.Deleted(
              base.getOrElse(oldKey, sys.error("Diff invariant error")))) ++
        sameKeys
          .map { key =>
            val oldVal = base.getOrElse(key, sys.error("Diff invariant error"))
            val newVal =
              compare.getOrElse(key, sys.error("Diff invariant error"))
            key -> dataDiff(oldVal, newVal)
          }
          .filter { case (_, diff) => diff.changed }

    if (changes.isEmpty) DataDiff.Unchanged else SetChanged(changes.toList)
  }

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
