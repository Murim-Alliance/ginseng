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
        val sifu     = alliance.recruit()

        patriarch.subjugate(disciple = disciple, metas = alliance.metas)
        patriarch.subjugate(disciple = sifu, metas = alliance.metas)

        patriarch.makeReceive(sifu = sifu, disciple = disciple, knowledge = "Cheese", metas = alliance.metas)
    }
}
