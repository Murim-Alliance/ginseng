package ginseng

import org.scalatest.funsuite.AnyFunSuite

class SectTest extends AnyFunSuite {
    test("Check if binary search works") {
        val allocator = Palace()
        val entity1   = allocator.recruit();
        val entity2   = allocator.recruit()

        val table1 = Sect()
        val table2 = table1.extendWith(entity1)

        val table3 = table2.extendWith(entity2)

        val table4 = table3.reduceWith(entity1)
        val table5 = table4.reduceWith(entity2)

        assert(table2.hasCourtyard(entity1))
        assert(table3.hasCourtyard(entity2))

        assert(!table4.hasCourtyard(entity1))

        assert(table4.hasCourtyard(entity2))

        assert(!table5.hasCourtyard(entity1))
        assert(!table5.hasCourtyard(entity2))
    }
}
