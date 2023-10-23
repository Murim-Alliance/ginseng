package ginseng

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

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
type Courtyards = OrderedUnique[(ScrollId, Courtyard)]

/**
 * Companion object for the Sect class.
 */
object Sect {
  /**
   * @return a new instance of Sect without any Courtyards.
   */
  def apply(): Sect = {
    Sect(OrderedUnique[(ScrollId, Courtyard)]())
  }

  /**
   * @param courtyards the courtyards to initialize the Sect with.
   * @return a new instance of Sect with the given Courtyards.
   */
  def apply(courtyards: Courtyards): Sect = {
    val scrolls: OrderedUnique[ScrollId] = courtyards.map(p => p._1)
    new Sect(courtyards, scrolls)
  }
}

/**
 * A Sect defines functionality to interact with collections of Courtyards.
 * The Sect allows for consistent modification across all Courtyards.
 *
 * @param courtyards the Courtyards of the Sect and the Scrolls that they teach.
 */
final class Sect private(val courtyards: Courtyards, val scrolls: OrderedUnique[ScrollId]) {

  /**
   * Returns an immutable indexes sequence of ScrollIds contained in the Sect.
   *
   * @return an immutable indexed sequence of scroll ids.
   */
  def exposeScrollIds: OrderedUnique[ScrollId] = this.scrolls

  /**
   * Adds a new Courtyard which teaches this Scroll.
   * The Courtyard is initialized with an empty ArrayBuffer.
   * NOTE: Do not push the same column twice.
   *
   * @param scrollId
   * @return
   */
  def extendWith(scrollId: ScrollId): Sect = {
    val newColumns = typeCloneEmpty()
    courtyards.binarySearch(scrollId) match {
      case Some(index) => newColumns.insert(index, (scrollId, mutable.ArrayBuffer.empty[Any]))
      case None => newColumns.addOne((scrollId, mutable.ArrayBuffer.empty[Any]))
    }

    Sect(newColumns)
  }

  /**
   * Creates a type clone of the Sect's Courtyards.
   * All the Courtyards of the clone will be empty.
   *
   * @return an empty type clone of the Sect's Courtyards.
   */
  private def typeCloneEmpty(): Courtyards = {
    this.courtyards.map(pair => (pair._1, mutable.ArrayBuffer.empty[Any]))
  }

  /**
   * Returns the amount of Disciples in the first Courtyard.
   *
   * @return the amount of Disciples in the first Courtyard.
   */
  def headCount(): Int = courtyards(0)._2.length

  /**
   * Makes a Sect stop practicing the given Scroll.
   * NOTE: Do not reduce Courtyards that do not contain the Scroll.
   *
   * @param scrollId the Scroll to stop practicing.
   * @return a Sect that is not practicing the Scroll.
   */
  def reduceWith(scrollId: ScrollId): Sect = {
    val to_reduce = typeCloneEmpty()
    to_reduce.remove(to_reduce.binarySearch(scrollId).get)
    Sect(to_reduce)
  }

  /**
   * Lossy transfers a row from the current Sect to another Sect.
   *
   * @param destinationSect the destination Sect.
   * @param hallId          the index of the Hall to transfer.
   */
  def lossyTransferHallTo(destinationSect: Sect, hallId: Int): Unit = {
    // Remove row in src
    val row: Map[ScrollId, Any] = this.courtyards.map(pair => (pair._1, pair._2.swapRemove(hallId))).toMap

    // Add to dst table
    destinationSect.courtyards.foreach(pair => {
      row.get(pair._1) match {
        case Some(value) => destinationSect.pushValue(pair._1, value)
        case None => ??? // implied reduction/lossy
      }
    })
  }

  /**
   * Adds a value to a Courtyard that teaches the given Scroll.
   * NOTE: Assumed that ScrollId exist/is valid.
   *
   * @param scrollId the Scroll the Courtyard teaches.
   * @param value    the value to add.
   */
  def pushValue(scrollId: ScrollId, value: Any): Unit = {
    val i = courtyards.binarySearch(scrollId).get
    courtyards(i)._2.addOne(value)
  }

  /**
   * Dumps the Hall which will automatically deallocate it.
   * This should only be done if the Disciple is being deallocated.
   * Meta should also be cleared.
   *
   * @param hallId the hall to dump.
   */
  def dumpHall(hallId: Int): Unit = {
    // Dumps the row which will automatically deallocate it
    // Should only be done if entity is being deallocated
    // Meta should also be cleared
    this.courtyards.foreach(_._2.swapRemove(hallId))
  }

  /**
   * Check if a Courtyard exist in the Sect that teaches the given Scroll.
   * e.g. Checks if the Sect practices the given Scroll.
   *
   * @param scrollId the scroll to check.
   * @return True if a Courtyard teaches the given Scroll, False otherwise.
   */
  def hasCourtyard(scrollId: ScrollId): Boolean = this.courtyards.binarySearch(scrollId).nonEmpty
}


extension[T] (array: mutable.ArrayBuffer[T])

  /**
   * Removes an element from the array at the given index and returns it.
   * NOTE: Assumes there is at least 1 row or more
   */
  def swapRemove(to_remove: Int): T = {
    if (array.length > 1) {
      val last = array.last
      val value = array(to_remove)
      array.update(to_remove, last)
      array.remove(array.length - 1)
      value
    } else {
      array.remove(0)
    }
  }


extension (courtyards: Courtyards)

  /**
   * Performs a binary search for a Courtyard with the given Scroll.
   */
  def binarySearch(scrollId: ScrollId): Option[CourtyardId] = {

    /**
     * Recursive binary search function.
     *
     * @param left  the left index of the search range
     * @param right the right index of the search range
     * @return an Option containing the index of the Courtyard if found, None otherwise
     */
    @tailrec
    def search(left: Int, right: Int): Option[CourtyardId] = {
      if (left > right) {
        None
      } else {
        val mid = (left + right) / 2
        val cmp = courtyards(mid)._1.id.compareTo(scrollId.id)
        if (cmp == 0 && courtyards(mid)._1.gen == scrollId.gen) {
          Some(mid)
        } else if (cmp < 0) {
          search(mid + 1, right)
        } else {
          search(left, mid - 1)
        }
      }
    }

    search(0, courtyards.length - 1)
  }
