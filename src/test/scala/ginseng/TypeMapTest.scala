package ginseng

import org.scalatest.funsuite.AnyFunSuite

class TypeMapTest extends AnyFunSuite {
	test("Get") {
		val map = TypeMap[Disciple]()
		map.register[String](Disciple(0, 0))
		map.register[Int](Disciple(100, 10))
		assert(map.getValue[String].get == Disciple(0, 0))
	}

}
