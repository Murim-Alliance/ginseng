package ginseng

import scala.collection.View
import scala.reflect.ClassTag

object Realm {
	def apply() = new Realm()
}

class Realm {
	/**
	 * TODO
	 */
	private val registry = TypeMap[Disciple]()

	/**
	 * TODO
	 */
	private val palace = Palace()

	/**
	 * TODO
	 */
	private val emperor = Emperor()

	def query[R <: NonEmptyTuple, F <: Tuple](): Array[R] = {
		// find the tables that contain all R but filter on F
		// (A, B: Hlist) => (BitSet, B: Hlist) => List[BitSet] => BitSet (reduce/fold it)
		// now we know which tables we have to get
		// get the list of the tables,
		// Consider List[R'] where R' is R prime
		// R' is a List for which the element is a tuple of which each element is a list of the type in tuple R
		// Now just provide a way to iterate over it or something

		???
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param t        TODO
	 * @tparam T TODO
	 */
	def setScrollOnDisciple[T: ClassTag](disciple: Disciple, t: T): Unit = {
		// check if disciple is alive
		if !palace.isActive(disciple) then return

		val scrollId = registry.getValue[T] match {
			case Some(d1) => d1
			case None =>
				val d1 = spawn()
				registry.register[T](d1)
				d1
		}
		emperor.teachScrollToDisciple(disciple, t, scrollId, palace.metas)
	}

	/**
	 * TODO
	 */
	def spawn(): Disciple = {
		// Disciple(0, 0) is already hardcoded to be the disciple disciple
		val newDisciple = palace.recruit()
		emperor.placeNewDisciple(disciple = newDisciple, metas = palace.metas)
		newDisciple
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param tag      TODO
	 */
	def setTagOnDisciple(disciple: Disciple, tag: Disciple): Unit = {

		// check if disciple is alive
		if !palace.isActive(disciple) then return
		else {
			// Unit value because it's a tag
			emperor.teachScrollToDisciple(disciple = disciple, scroll = (), scrollId = tag, palace.metas)
		}
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param tag      TODO
	 * @tparam T TODO
	 */
	def unsetTagOnDisciple[T](disciple: Disciple, tag: Disciple): Unit = {
		if !palace.isActive(disciple) then ()
		else {
			emperor.forgetScrollForDisciple(disciple, tag, palace.metas)
		}
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param scrollId TODO
	 * @tparam T TODO
	 */
	def unsetScrollOnDisciple[T: ClassTag](disciple: Disciple, scrollId: ScrollId): Unit = {
		if !palace.isActive(disciple) then ()
		else {
			val scrollId = registry.getValue[T] match {
				case Some(d1) => d1
				case None => return
			}
			emperor.forgetScrollForDisciple(disciple, scrollId, palace.metas)
		}
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @tparam T TODO
	 * @return TODO
	 */
	def getScrollFromDisciple[T: ClassTag](disciple: Disciple): Option[T] = {
		if !palace.isActive(disciple) then return None
		val scrollId = registry.getValue[T] match {
			case Some(scroll) => scroll
			case None => return None
		}
		// Must be some
		val (sect, hall): (Sect, HallId) = palace.metas(disciple.id).get
		sect.getScrollValue(scrollId, hall).map(_.asInstanceOf[T])
	}
}
