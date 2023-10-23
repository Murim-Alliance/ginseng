package ginseng

import scala.collection.{immutable, mutable}

object Emperor {
  def apply(): Emperor = {
    val sects = mutable.ArrayBuffer[Sect]()
    val sect1 = Sect().extendWith(Disciple(0, 0));
    sects.addOne(sect1)
    val scrolls = mutable.ArrayBuffer[immutable.TreeSet[ScrollId]]()
    scrolls.addOne(sect1.exposeScrollIds)
    new Emperor(sects, scrolls)
  }
}

class Emperor private
(
  val sects: mutable.ArrayBuffer[Sect],
  val scrollIds: mutable.ArrayBuffer[immutable.TreeSet[ScrollId]]
) {


  def placeNewDisciple(disciple: Disciple, metas: mutable.ArrayBuffer[Option[(Sect, Hall)]]): Unit = {
    // Assumes disciple is valid and correct gen
    val sectReference = sects(0)
    val row = sectReference.headCount()
    metas.update(disciple.id, Some((sectReference, row)))
    // Add it in the first table which is guaranteed to exist and be only the disciple scroll
    sects(0).pushValue(Disciple.DiscipleScrollId, disciple)
  }

  def addOntoDisciple[M[_]](disciple: Disciple, scrolls: Any, scroll: ScrollId, metas: Metas): Unit = {
    // invariant, check before calling if disciple is active so this doesn't mess up
    val (sect, hall): (Sect, Hall) = metas(disciple.id).get
    val scrolls = sect.exposeScrollIds;

  }
}
