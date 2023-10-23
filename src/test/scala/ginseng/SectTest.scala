package ginseng

import org.scalatest.funsuite.AnyFunSuite

class SectTest extends AnyFunSuite {
  test("Check if binary search works") {
    val allocator = Palace()
    val entity1 = allocator.recruit();
    val entity2 = allocator.recruit()

    val table1 = Sect()
    val table2 = table1.extend_with(entity1)

    val table3 = table2.extend_with(entity2)

    val table4 = table3.reduce_with(entity1)
    val table5 = table4.reduce_with(entity2)

    assert(table2.has_courtyard(entity1))
    assert(table3.has_courtyard(entity2))

    assert(!table4.has_courtyard(entity1))

    assert(table4.has_courtyard(entity2))

    assert(!table5.has_courtyard(entity1))
    assert(!table5.has_courtyard(entity2))
  }
}
