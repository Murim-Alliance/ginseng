package ginseng

import scala.annotation.tailrec
import scala.collection.immutable

object OrderedUnique {
  /**
   * @param ordering an implicit ordering of element of type T.
   * @tparam T the type of the elements in the collection.
   * @return a new instance of OrderedUnique with an empty collection of elements of type T.
   */
  def apply[T](implicit ordering: Ordering[T]): OrderedUnique[T] = new OrderedUnique[T](immutable.IndexedSeq[T]())
}

/**
 * Represents and ordered and unique collection of elements of type T.
 *
 * @param collection an immutable indexed sequence of elements of type T.
 * @param ordering   an implicit ordering of elements of type T.
 * @tparam T the type of the elements in the collection.
 */
final class OrderedUnique[T] private(val collection: immutable.IndexedSeq[T] = immutable.IndexedSeq[T]())(implicit ordering: Ordering[T]) extends PartialFunction[Int, T] {

  /**
   * Adds an element to the collection while maintaining the order and uniqueness of elements.
   *
   * @param t the element to be added to the collection.
   * @return a new OrderedUnique instance with the added element.
   */
  def push(t: T): OrderedUnique[T] = {
    val result = collection.toBuffer[T]

    binarySearch(t) match {
      case Some(index) => result.insert(index, t)
      case None => ???
    }

    new OrderedUnique[T](result.toIndexedSeq)
  }

  /**
   * Checks if an element is present in the collection.
   *
   * @param t the element to be checked for the presence in the collection.
   * @return True if the element is present in the collection, False otherwise.
   */
  def contains(t: T): Boolean = {
    binarySearch(t).isDefined
  }

  /**
   * Removes an element from the collection while maintaining the order and the uniqueness of elements.
   *
   * @param t the element to be removed from the collection.
   * @return a new OrderedUnique instance with the removed element.
   */
  def remove(t: T): OrderedUnique[T] = {
    val result = collection.toBuffer[T]

    binarySearch(t) match {
      case Some(index) => result.remove(index)
      case None => ???
    }

    new OrderedUnique[T](result.toIndexedSeq)
  }

  def map[B](f: T => B)(implicit ordering: Ordering[B]): OrderedUnique[B] = new OrderedUnique[B](collection = collection.map(f))

  /**
   * Performs a binary search on the collection to find the index of an element.
   *
   * @param t the element to be searched for in the collection.
   * @return Some(index) if the element is present in the collection, None otherwise.
   */
  private def binarySearch(t: T): Option[Int] = {

    /**
     * Recursive binary search function.
     *
     * @param left  the left index of the search range
     * @param right the right index of the search range
     * @return an Option containing the index of the Courtyard if found, None otherwise
     */
    @tailrec
    def search(left: Int, right: Int): Option[Int] = {
      if (left > right) {
        None
      } else {
        val mid = (left + right) / 2
        val cmp = ordering.compare(t, collection(mid))
        if (cmp == 0) {
          Some(mid)
        } else if (cmp < 0) {
          search(left, mid - 1)
        } else {
          search(mid + 1, right)
        }
      }
    }

    search(0, collection.size - 1)
  }

  override def isDefinedAt(x: Int): Boolean = collection.lift(x).isDefined

  override def apply(v1: Int): T = collection(v1)
}

