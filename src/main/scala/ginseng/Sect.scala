package ginseng

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

/**
 * Type alias for HallId, which is an Int.
 */
type HallId = Int

/**
 * Type alias for ScrollId, which is a Disciple.
 */
type ScrollId = Disciple

/**
 * Type alias for a CourtyardId, which is an Int.
 */
opaque type CourtyardId = Int

/**
 * Type alias for Courtyard, which is a mutable ArrayBuffer of type Any.
 */
type Courtyard = mutable.ArrayBuffer[Any]

/**
 * Type alias for Courtyards, which represents the collection of Courtyards and the Scrolls that they teach.
 */
type Courtyards = immutable.TreeMap[ScrollId, Courtyard]

/**
 * Companion object for the Sect class.
 */
object Sect {
	/**
	 * @return a new instance of Sect without any Courtyards.
	 */
	def apply(): Sect = {
		Sect(immutable.TreeMap[ScrollId, Courtyard]())
	}

	/**
	 * @param courtyards the courtyards to initialize the Sect with.
	 * @return a new instance of Sect with the given Courtyards.
	 */
	def apply(courtyards: Courtyards): Sect = {
		val scrolls: immutable.TreeSet[ScrollId] = courtyards.keySet
		new Sect(courtyards, scrolls)
	}
}

/**
 * A Sect defines functionality to interact with collections of Courtyards.
 * The Sect allows for consistent modification across all Courtyards.
 *
 * @param courtyards the Courtyards of the Sect and the Scrolls that they teach.
 */
private[ginseng] final class Sect(private val courtyards: Courtyards, private val scrolls: immutable.TreeSet[ScrollId]) {

	/**
	 * Returns an immutable indexes sequence of ScrollIds contained in the Sect.
	 *
	 * @return an immutable indexed sequence of scroll ids.
	 */
	@inline def exposeScrollIds: immutable.TreeSet[ScrollId] = this.scrolls

	/**
	 * Adds a new Courtyard which teaches this Scroll.
	 * The Courtyard is initialized with an empty ArrayBuffer.
	 * NOTE: Do not push the same column twice.
	 *
	 * @param scrollId the Scroll to teach.
	 * @return the Sect which additionally teaches the new Scroll.
	 */
	@inline def extendWith(scrollId: ScrollId): Sect =
		Sect(typeCloneEmpty().updated(scrollId, mutable.ArrayBuffer.empty[Any]))

	/**
	 * Returns the amount of Disciples in the first Courtyard.
	 *
	 * @return the amount of Disciples in the first Courtyard.
	 */
	@inline def headCount(): Int = courtyards(Disciple.DiscipleScrollId).length

	/**
	 * Makes a Sect stop practicing the given Scroll.
	 * NOTE: Do not reduce Courtyards that do not contain the Scroll.
	 *
	 * @param scrollId the Scroll to stop practicing.
	 * @return a Sect that is not practicing the Scroll.
	 */
	@inline def reduceWith(scrollId: ScrollId): Sect =
		Sect(typeCloneEmpty().removed(scrollId))

	/**
	 * Creates a type clone of the Sect's Courtyards.
	 * All the Courtyards of the clone will be empty.
	 *
	 * @return an empty type clone of the Sect's Courtyards.
	 */
	@inline private def typeCloneEmpty(): Courtyards =
		this.courtyards.map(pair => (pair._1, mutable.ArrayBuffer[Any]()))(ordering = courtyards.ordering)

	/**
	 * Lossy transfers a row from the current Sect to another Sect.
	 *
	 * @param destinationSect the destination Sect.
	 * @param hallId          the index of the Hall to transfer.
	 */
	def lossyTransferHallTo(destinationSect: Sect, hallId: HallId): Unit = {
		// Remove row in src
		val row: Map[ScrollId, Any] = this.courtyards.iterator.map(p => (p._1, p._2.swapRemove(hallId))).toMap
		println(row)
		// Add to dst table
		destinationSect.courtyards.foreach(pair => {
			row.get(pair._1) match {
				case Some(value) => destinationSect.pushValue(pair._1, value)
				case None => // implied reduction/lossy
			}
		})
	}

	/**
	 * Write a new value on a new instance of the Scroll taught at a Courtyard.
	 * NOTE: Assumed that ScrollId exist/is valid.
	 *
	 * @param scrollId the Scroll the Courtyard teaches.
	 * @param value    the value to write.
	 */
	@inline def pushValue(scrollId: ScrollId, value: Any): Unit =
		courtyards.get(scrollId) match {
			case Some(courtyard) => courtyard.addOne(value)
			case None => ???
		}

	/**
	 * Writes a value on the Scroll for a specific Hall.
	 *
	 * @param scrollId the Scroll to change the value of.
	 * @param value    the value to set the value of the scroll to.
	 * @param hall     the specific hall to update the value for
	 */
	@inline def replaceScrollValue(scrollId: ScrollId, value: Any, hall: HallId): Unit =
		courtyards.get(scrollId) match {
			case Some(courtyard) => courtyard.update(hall, value)
			case None => ???
		}


	/**
	 * Dumps the Hall which will automatically deallocate it.
	 * This should only be done if the Disciple is being deallocated.
	 * Meta should also be cleared.
	 *
	 * @param hallId the hall to dump.
	 */
	@inline def dumpHall(hallId: HallId): Unit =
		this.courtyards.foreach(_._2.swapRemove(hallId))

	/**
	 * Check if a Courtyard exist in the Sect that teaches the given Scroll.
	 * e.g. Checks if the Sect practices the given Scroll.
	 *
	 * @param scrollId the scroll to check.
	 * @return True if a Courtyard teaches the given Scroll, False otherwise.
	 */
	@inline def hasCourtyard(scrollId: ScrollId): Boolean = this.courtyards.contains(scrollId)
}


extension (array: Courtyard)

	/**
	 * Removes an element from the array at the given index and returns it.
	 * NOTE: Assumes there is at least 1 row or more
	 */
	def swapRemove(to_remove: Int): Any =
		if (array.length > 1) {
			val last = array.last
			val value = array(to_remove)
			array.update(to_remove, last)
			array.remove(array.length - 1)
			value
		} else {
			array.remove(0)
		}

