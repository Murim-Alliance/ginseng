package ginseng

import scala.reflect.ClassTag

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

	def setTagOnDisciple(disciple: Disciple, tag: Disciple): Unit = {

		// check if disciple is alive
		if !palace.isActive(disciple) then return
		else {
		// Unit value because it's a tag
			emperor.teachScrollToDisciple(disciple = disciple, scroll = (), scrollId = tag, palace.metas)
		}
	}

	def unsetTagOnDisciple[T](disciple: Disciple, tag: Disciple): Unit = {
		if !palace.isActive(disciple) then ()
		else {
			emperor.forgetScrollForDisciple(disciple, tag, palace.metas)
		}
	}

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
	 */
	def spawn(): Disciple = {
		// Disciple(0, 0) is already hardcoded to be the disciple disciple
		val newDisciple = palace.recruit()
		emperor.placeNewDisciple(disciple = newDisciple, metas = palace.metas)
		newDisciple
	}


}
