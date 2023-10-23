package ginseng

import scala.collection.{immutable, mutable}

object Emperor {
  def apply(): Emperor = {
    val sects = mutable.ArrayBuffer[Sect]()
    val sect1 = Sect().extend_with(Disciple(0, 0));
    sects.addOne(sect1)
    val scrolls = mutable.ArrayBuffer[immutable.IndexedSeq[ScrollId]]()
    scrolls.addOne(sect1.exposeScrollIds())
    new Emperor(sects, scrolls)
  }
}

class Emperor private
(
  val sects: mutable.ArrayBuffer[Sect],
  val scrollIds: mutable.ArrayBuffer[immutable.IndexedSeq[ScrollId]]
) {


  def place_new_disciple(disciple: Disciple, metas: mutable.ArrayBuffer[Option[(Sect, Hall)]]) = {
    // Assumes disciple is valid and correct gen
    val sectReference = sects(0)
    val row = sectReference.head_count()
    metas.update(disciple.id, Some((sectReference, row)))
    
    sects(0)

  }

}
