package ginseng

import org.scalatest.funsuite.AnyFunSuite

class PatriarchTest extends AnyFunSuite {
    test("Patriarch: Assign") {
        val patriarch = Patriarch()
        val alliance  = Alliance()

        val disciple = alliance.recruit()

        patriarch.subjugate(disciple = disciple, metas = alliance.metas)
    }

    test("Patriarch: Receive") {
        val patriarch = Patriarch()
        val alliance  = Alliance()

        val disciple = alliance.recruit()
        val teacher     = alliance.recruit()

        patriarch.subjugate(disciple = disciple, metas = alliance.metas)
        patriarch.subjugate(disciple = teacher, metas = alliance.metas)

        patriarch.makeReceive(teacher = teacher, disciple = disciple, knowledge = "Cheese", metas = alliance.metas)
    }
}
