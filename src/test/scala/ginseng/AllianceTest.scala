package ginseng

import org.scalatest.funsuite.AnyFunSuite

class AllianceTest extends AnyFunSuite {
    test("Alliance: Allocation") {
        val alliance = Alliance()
        val disciple = alliance.recruit()

        assert(disciple != Disciple(0, 0))
        assert(disciple != alliance.recruit())
    }
}
