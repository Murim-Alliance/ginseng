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

	/**
	 * TODO
	 */
	def spawn(): Disciple = {
		// Disciple(0, 0) is already hardcoded to be the disciple disciple
		val newDisciple = palace.recruit()
		emperor.placeNewDisciple(disciple = newDisciple, metas = palace.metas)
		newDisciple
	}

	def setOnDisciple[K: ClassTag](disciple: Disciple, t: K): Unit = {
		val scrollId = registry.getValue[K] match {
			case Some(d1) => d1
			case None =>
				val d1 = spawn()
				registry.register[K](d1)
				d1
		}
	}

	def setOnDisciple(disciple: Disciple, tag: Disciple): Unit = {

	}
}
