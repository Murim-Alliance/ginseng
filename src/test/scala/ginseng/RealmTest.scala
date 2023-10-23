package ginseng

import org.scalatest.funsuite.AnyFunSuite
import org.scalactic.Prettifier.default

class RealmTest extends AnyFunSuite {
		test("ez pz testy squeezy") {
			val realm = Realm()

			val d1 = realm.spawn()
			val d2 = realm.spawn()

			realm.setScrollOnDisciple(d1, "Cheese")
			realm.setScrollOnDisciple(d2, 1)

			assert(realm.getScrollFromDisciple[String](d1).get == "Cheese")
			assert(realm.getScrollFromDisciple[String](d2).isEmpty)
			assert(realm.getScrollFromDisciple[Int](d2).get == 1)

		}
}
