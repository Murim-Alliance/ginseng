package ginseng


/**
 * Represents a Disciple with a unique identifier and generation number.
 *
 * @param id  The unique identifier of the Disciple.
 * @param gen The generation of the Disciple, used to track the lifetime of the Disciple
 *            When an Disciple is deallocated, its generation is incremented, which invalidates any references to the Disciple.
 *            When an Disciple is allocated, its generation is set to 0, which means that the Disciple is alive and can be used.
 */
case class Disciple(id: Int, gen: Int)

object Disciple {
  val DiscipleScrollId: Disciple = Disciple(0, 0)

  implicit val discipleOrdering: Ordering[Disciple] = (x: Disciple, y: Disciple) => x.id.compareTo(y.id)
}
