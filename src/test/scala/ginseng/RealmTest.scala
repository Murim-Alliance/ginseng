package ginseng

import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class RealmTest extends AnyFunSuite {
    test("ez pz testy squeezy") {
        val realm = Realm()

        val d1 = realm.reincarnate()
        val d2 = realm.reincarnate()

        realm.teachElementalScroll[Int](d1, 2)
        realm.teachElementalScroll(d1, this)
        realm.teachMartialScroll(d2, d1)

        assert(realm.inquireElementalKnowledge[Int](d1).get == 2)
        assert(realm.inquireElementalKnowledge[String](d2).isEmpty)
        assert(realm.inquireMartialKnowledge(d2, d1))
    }
}
