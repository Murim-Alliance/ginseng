package ginseng

object Disciple {

    /**
     * The Ancestor Teacher that represents the Entity(0, 0).
     */
    val AncestorTeacherId: Disciple = Disciple(0, 0)

    /**
     * Ordering for Disciples.
     * Disciples are ordered by their unique identifier.
     */
    implicit val discipleOrdering: Ordering[Disciple] = (x: Disciple, y: Disciple) => x.id.compareTo(y.id)
}

/**
 * Represents a Disciple with a unique identifier and generation number.
 *
 * @param id  The unique identifier of the Disciple.
 * @param gen The generation of the Disciple, used to track the lifetime of the Disciple
 *            When an Disciple is deallocated, its generation is incremented, which invalidates any references to the Disciple.
 *            When an Disciple is allocated, its generation is set to 0, which means that the Disciple is alive and can be used.
 */
case class Disciple(id: Int, gen: Int)
