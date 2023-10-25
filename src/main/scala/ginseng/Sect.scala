package ginseng

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

/**
 * Type alias for HallId, which is an Int.
 */
type HallId = Int

/**
 * Type alias for SifuId, which is a Disciple.
 */
type SifuId = Disciple

/**
 * Type alias for a CourtyardId, which is an Int.
 */
opaque type CourtyardId = Int

/**
 * Type alias for Knowledge, which is Any.
 */
type Knowledge = Any

/**
 * Type alias for Courtyard, which is a mutable ArrayBuffer of type Any.
 */
type Courtyard = mutable.ArrayBuffer[Knowledge]

/**
 * Type alias for Courtyards, which represents the collection of which Sifu teaches at which Courtyard
 */
type Courtyards = immutable.TreeMap[SifuId, Courtyard]

/**
 * Companion object for the Sect class.
 */
object Sect {

    /**
     * @return a new instance of Sect without any Courtyards.
     */
    def apply(): Sect = {
        Sect(immutable.TreeMap[SifuId, Courtyard]())
    }

    /**
     * @param courtyards the courtyards to initialize the Sect with.
     * @return a new instance of Sect with the given Courtyards.
     */
    def apply(courtyards: Courtyards): Sect = {
        val scrolls: immutable.TreeSet[SifuId] = courtyards.keySet
        new Sect(courtyards, scrolls)
    }
}

/**
 * A Sect defines functionality to interact with collections of Courtyards.
 * The Sect allows for consistent modification across all Courtyards.
 *
 * @param courtyards the Courtyards of the Sect and the Scrolls that they teach.
 */
private[ginseng] final class Sect(
    private val courtyards: Courtyards,
    private val scrolls: immutable.TreeSet[SifuId]
) {

    /**
     * Returns an immutable indexes sequence of ScrollIds contained in the Sect.
     *
     * @return an immutable indexed sequence of scroll ids.
     */
    @inline def sifus: immutable.TreeSet[SifuId] = this.scrolls

    /**
     * Hires a Sifu to teach at a new Courtyard in the Sect.
     * The Courtyard is initialized with an empty ArrayBuffer.
     *
     * NOTE: Do not push the same column twice.
     *
     * @param sifuId the Sifu who teaches here
     * @return the Sect which additionally teaches the new Scroll.
     */
    @inline def hire(sifuId: SifuId): Sect =
        Sect(blueprint().updated(sifuId, mutable.ArrayBuffer.empty[Any]))

    /**
     * Returns the amount of Disciples in the first Courtyard.
     *
     * @return the amount of Disciples in the first Courtyard.
     */
    @inline def headCount(): Int = courtyards(Disciple.AncestorSifuId).length

    /**
     * Retire a Sifu from their position, abolishing their Courtyard.
     *
     * NOTE: Do not reduce Courtyards that do not contain the Scroll.
     *
     * @param sifuId the Sifu to retire.
     * @return a Sect where the Sifu does not teach at.
     */
    @inline def retire(sifuId: SifuId): Sect =
        Sect(blueprint().removed(sifuId))

    /**
     * Creates a type clone of the Sect's Courtyards.
     * All the Courtyards of the clone will be empty.
     *
     * @return an empty type clone of the Sect's Courtyards.
     */
    @inline private def blueprint(): Courtyards =
        this.courtyards.map(pair => (pair._1, mutable.ArrayBuffer[Any]()))(ordering = courtyards.ordering)

    /**
     * Lossy transfers a row from the current Sect to another Sect.
     *
     * @param hallId          the index of the Hall to transfer.
     * @param destinationSect the destination Sect.
     */
    def transferToOtherSect(hallId: HallId, destinationSect: Sect): Unit = {
        // Remove row in src
        val row: Map[SifuId, Any] = this.courtyards.iterator.map(p => (p._1, p._2.swapRemove(hallId))).toMap

        // Add to dst table
        destinationSect.courtyards.foreach(pair => {
            row.get(pair._1) match {
                case Some(value) => destinationSect.induction(pair._1, value)
                case None        => // implied reduction/lossy
            }
        })
    }

    /**
     * Make a Sifu teach Knowledge to the newest Disciple.
     *
     * NOTE: Assumed that SifuId exist/is valid.
     *
     * @param sifuId    the Sifu to teach the Knowledge.
     * @param knowledge the Knowledge to teach.
     */
    @inline def induction(sifuId: SifuId, knowledge: Knowledge): Unit =
        courtyards.get(sifuId) match {
            case Some(courtyard) => courtyard.addOne(knowledge)
            case None            => ???
        }

    /**
     * Make a Sifu tutor a specific disciple with some Knowledge.
     *
     * @param sifuId   the Sifu to teach the Knowledge.
     * @param value    the new Knowledge to teach.
     * @param hall     the Hall the disciple to tutor is at.
     */
    @inline def tutor(sifuId: SifuId, value: Knowledge, hall: HallId): Unit =
        courtyards.get(sifuId) match {
            case Some(courtyard) => courtyard.update(hall, value)
            case None            => ???
        }

    /**
     * Inquire the Knowledge the Sifu is currently teaching to a Disciple.
     *
     * @param sifuId   a Sifu who teaches at the Sect.
     * @param hallId   the specific hall of the Disciple to inquire about.
     * @return         the Knowledge the Sifu is teacher the Disciple.
     */
    @inline def inquire(sifuId: SifuId, hallId: HallId): Option[Any] =
        courtyards.get(sifuId).map(_(hallId))

    /**
     * Exiles the Disciple in the Hall.
     *
     * NOTE: Dumps the Hall which will automatically deallocate it.
     *       This should only be done if the Disciple is being deallocated.
     *       Meta should also be cleared.
     *
     * @param hallId the hall to dump.
     */
    @inline def exile(hallId: HallId): Unit =
        this.courtyards.foreach(_._2.swapRemove(hallId))

    /**
     * Check if the Sect has a Courtyard where a specific Sifu teaches at.
     *
     * @param sifuId the Sifu to check for.
     * @return True if a Courtyard exists, False otherwise.
     */
    @inline def hasCourtyard(sifuId: SifuId): Boolean = this.courtyards.contains(sifuId)
}

extension (array: Courtyard)
    /**
     * Removes an element from the array at the given index and returns it.
     *
     * NOTE: Assumes there is at least 1 row or more
     */
    def swapRemove(to_remove: Int): Any =
        if (array.length > 1) {
            val last  = array.last
            val value = array(to_remove)
            array.update(to_remove, last)
            array.remove(array.length - 1)
            value
        } else {
            array.remove(0)
        }
