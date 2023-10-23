package ginseng

import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class RealmTest extends AnyFunSuite {
	test("ez pz testy squeezy") {
		val realm = Realm()

		val d1 = realm.spawn()
		val d2 = realm.spawn()
		realm.setScrollOnDisciple(d1, this)
		realm.setScrollOnDisciple(d2, d1)
		//		realm.getScrollFromDisciple(d1).get.setScrollOnDisciple(d1, "")

		//		assert(realm.getScrollFromDisciple[String](d1).get == "")
		assert(realm.getScrollFromDisciple[String](d2).isEmpty)
		assert(realm.getScrollFromDisciple[Disciple](d2).get == d1)
	}
}
