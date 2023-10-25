package ginseng

import org.scalatest.funsuite.AnyFunSuite

class SectTest extends AnyFunSuite {
    test("Sect: Hire/Retire") {
        val alliance = Alliance()
        val entity1  = alliance.recruit()
        val entity2  = alliance.recruit()

        val table1 = Sect()
        val table2 = table1.hire(entity1)
        val table3 = table2.hire(entity2)
        val table4 = table3.retire(entity1)
        val table5 = table4.retire(entity2)

        assert(table2.hasCourtyard(entity1))
        assert(table3.hasCourtyard(entity2))
        assert(!table4.hasCourtyard(entity1))
        assert(table4.hasCourtyard(entity2))
        assert(!table5.hasCourtyard(entity1))
        assert(!table5.hasCourtyard(entity2))
    }
}
