package ginseng

import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class RealmTest extends AnyFunSuite {
    test("Realm: Teach") {
        val realm = Realm()

        val d1 = realm.recruit()

        realm.teach[Int](disciple = d1, 400)
        assert(realm.inquire[Int](disciple = d1).get == 400)
    }

    test("Realm: Class Teach") {
        val realm = Realm()

        val d1 = realm.recruit()
        case class Cheese(hp: Int)

        val cheese = Cheese(0)

        realm.teach[Cheese](disciple = d1, cheese)
        realm.teach[Int](disciple = d1, 400)

        assert(realm.inquire[Cheese](disciple = d1).get == cheese)
        assert(realm.inquire[Int](disciple = d1).get == 400)
    }

    test("Realm: Receive") {
        val realm = Realm()

        val d1 = realm.recruit()
        val d2 = realm.recruit()

        realm.receive(teacher = d2, disciple = d1)
        assert(realm.inquire(teacher = d2, disciple = d1))
    }
}
