package ginseng

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

type ScrollId = Disciple
type Courtyards = mutable.ArrayBuffer[(ScrollId, mutable.ArrayBuffer[Any])]

object Sect {
  def apply() = new Sect()
}

class Sect private(val courtyards: Courtyards = mutable.ArrayBuffer()) {

  def exposeScrollIds(): immutable.IndexedSeq[ScrollId] = {
    this.courtyards.map(p => p._1).toIndexedSeq
  }

  def extend_with(scrollId: ScrollId): Sect = {
    // Do not push the same column twice

    val newColumns = medium_clone()
    courtyards.binarySearch(scrollId) match {
      case Some(index) => newColumns.insert(index, (scrollId, mutable.ArrayBuffer.empty[Any]))
      case None => newColumns.addOne((scrollId, mutable.ArrayBuffer.empty[Any]))
    }
    new Sect(newColumns)
  }

  def head_count(): Int = courtyards(0)._2.length
  def reduce_with(scrollId: ScrollId): Sect = {
    // Do not reduce tables that do not contain the component

    val to_reduce = medium_clone()
    to_reduce.remove(to_reduce.binarySearch(scrollId).get)
    new Sect(to_reduce)
  }

  private def medium_clone(): Courtyards = {
    this.courtyards.map(pair => (pair._1, mutable.ArrayBuffer.empty[Any]))
  }

  def lossy_transfer_hall_to(dst_sect: Sect, current_hall: Int): Unit = {
    // Remove row in src
    val row: Map[ScrollId, Any] = this.courtyards.map(pair => (pair._1, pair._2.swap_remove(current_hall))).toMap

    // Add to dst table
    dst_sect.courtyards.foreach(pair => {
      row.get(pair._1) match {
        case Some(value) => dst_sect.push_value(pair._1, value)
        case None => ??? // implied reduction/lossy
      }
    })
  }

  private def push_value(scrollId: ScrollId, value: Any): Unit = {
    // assumes ComponentId exists/is valid
    val i = courtyards.binarySearch(scrollId).get
    courtyards(i)._2.addOne(value)
  }

  def dump_hall(row: Int): Unit = {
    // Dumps the row which will automatically deallocate it
    // Should only be done if entity is being deallocated
    // Meta should also be cleared
    this.courtyards.foreach(_._2.swap_remove(row))
  }

  def has_courtyard(scrollId: ScrollId): Boolean = this.courtyards.binarySearch(scrollId).nonEmpty
}


extension[T] (array: mutable.ArrayBuffer[T])

  /*
  Assumes there is at least 1 row or more
   */
  def swap_remove(to_remove: Int): T = {
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
  def binarySearch(scrollId: ScrollId): Option[Int] = {
    @tailrec
    def search(left: Int, right: Int): Option[Int] = {
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
