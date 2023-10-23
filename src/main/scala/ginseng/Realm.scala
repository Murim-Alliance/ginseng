package ginseng

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
	def spawn(): Unit = {
		// Disciple(0, 0) is already hardcoded to be the disciple disciple
		val newDisciple = palace.recruit()
		emperor.placeNewDisciple(disciple = newDisciple, metas = palace.metas)

		// 
	}
}
