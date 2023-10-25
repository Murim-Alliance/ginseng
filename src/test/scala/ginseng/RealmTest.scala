package ginseng

import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class RealmTest extends AnyFunSuite {
    test("Realm: Teach") {
        val realm = Realm()

        val d1 = realm.recruit()
        val d2 = realm.recruit()

        realm.teach[Int](disciple = d1, 2)

        assert(realm.inquire[Int](disciple = d1).get == 2)
        assert(realm.inquire[Int](disciple = d2).isEmpty)
    }

    test("Realm: Receive") {
        val realm = Realm()

        val d1 = realm.recruit()
        val d2 = realm.recruit()

        realm.receive(sifu = d2, disciple = d1)

        assert(realm.inquire(sifu = d2, disciple = d1))
    }
}
