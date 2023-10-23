package ginseng

import org.scalatest.funsuite.AnyFunSuite

class PalaceTest extends AnyFunSuite {
  test("Allocations should be unique and not Entity(0, 0)") {
    val palace = Palace()
    val disciple = palace.recruit();
    assert(disciple != Disciple(0, 0));
    assert(disciple != palace.recruit())
  }
}
