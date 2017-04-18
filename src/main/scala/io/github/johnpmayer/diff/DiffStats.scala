package io.github.johnpmayer.diff

import scalaz.Monoid

object DiffStats extends Monoid[DiffStats] {
  override val zero = DiffStats(None, None, None)
  val addedOne = DiffStats(added = Some(1), deleted = None, changed = None)
  val deletedOne = DiffStats(added = None, deleted = Some(1), changed = None)
  val changedOne = DiffStats(added = None, deleted = None, changed = Some(1))

  private def appendCountPart(f1: Option[BigInt],
                              f2: Option[BigInt]): Option[BigInt] =
    (f1, f2) match {
      case (None, None) => None
      case (None, n: Some[_]) => n
      case (n: Some[_], None) => n
      case (Some(n1), Some(n2)) => Some(n1 + n2)
    }

  override def append(f1: DiffStats, f2: => DiffStats): DiffStats =
    (f1, f2) match {
      case (DiffStats(a1, d1, c1), DiffStats(a2, d2, c2)) =>
        DiffStats(
          added = appendCountPart(a1, a2),
          deleted = appendCountPart(d1, d2),
          changed = appendCountPart(c1, c2)
        )
    }
}

case class DiffStats(added: Option[BigInt], deleted: Option[BigInt], changed: Option[BigInt])