package ginseng

import org.scalatest.funsuite.AnyFunSuite

class EmperorTest extends AnyFunSuite {
	test("Instantiate Emperor") {
		val emperor = Emperor()
		val palace = Palace()

		val e1 = palace.recruit();
		val e2 = palace.recruit();


		val scroll1 = palace.recruit()
		val scroll2 = palace.recruit()
		val scroll3 = palace.recruit()

		emperor.placeNewDisciple(disciple = e1, metas = palace.metas)
		emperor.placeNewDisciple(disciple = e2, metas = palace.metas)
		emperor.placeNewDisciple(scroll1, palace.metas)
		emperor.placeNewDisciple(scroll2, palace.metas)
		emperor.placeNewDisciple(scroll3, palace.metas)

		emperor.teachScrollToDisciple(disciple = e1, scroll = "Cheese", scrollId = scroll1, metas = palace.metas)
		emperor.teachScrollToDisciple(disciple = e1, scroll = 1, scrollId = scroll2, metas = palace.metas)
	}
}
