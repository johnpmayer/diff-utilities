package diff

trait OpaqueDiff[T] extends Diff[T]

object OpaqueDiff {

  case object Unchanged extends OpaqueDiff[Nothing] {
    override def stats: DiffStats = DiffStats.zero
    override def changed: Boolean = false
  }

  case class Replaced[T](t: T) extends OpaqueDiff[T] {
    override def stats: DiffStats = DiffStats.changedOne
    override def changed: Boolean = true
  }

}