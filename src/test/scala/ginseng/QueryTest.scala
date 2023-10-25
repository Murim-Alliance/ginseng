package ginseng

import org.scalatest.funsuite.AnyFunSuite

class QueryTest extends AnyFunSuite {
    test("Query: Unfiltered") {
        val realm = Realm()

        val d1 = realm.recruit()
        val d2 = realm.recruit()

        realm.teach[Int](d1, 2)
        realm.teach[String](d1, "donut blaster")

        realm.teach[Int](d2, 2)
        realm.teach[Boolean](d2, false)

        type Query1 = Query[Read[Int], Without[String]]
        type Query2 = Query[Write[String], With[String]]

        // val q = Query[Int, None](realm).addFilter()
        realm.query[Query1#Search, Query1#Filter, Query1]()
        realm.query[Query2#Search, Query2#Filter, Query2]()
    }

    // IF YOU CANT HANDLE ME AT MY LOIS, YOU DONT DESERVE ME AT MY FAMILY GUY
}
