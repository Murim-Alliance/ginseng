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

        type Query1 = Query[Int, Unfiltered]
        type Query2 = Query[String, Unfiltered]

        // GUYS WELCOME BACK TO HOTLINE MIAMI REVIEW TODAY WE WILL BE TALKING ABOUT JASON LEBRONE
        // HE IS TRULY MY FAVORITE CHARACTER LIKE WHEN HE WENT INTO THE TOMATO AND ATE IT AND THEN
        // TALKED TO BIG JIMMY (HE HAS SUCH A COOL VOLKWAGEN 41A) THEN BIG JIMMY CAME UP AND LIKE HE
        // ORDERED BURGER?? ORDERED BURGERY IN HIS NEIGHBORHOOD!! WHAT!! HOW? WDYM BIG JIMMY
        // WOULD NEVER ORDER BURGER HE HAS ALWAYS BEEN A MAN OF COLD HARD LETTUCE AND I KNOW IT
        // I HAVE FOLLOWED EVERY SINGLE ONE OF BIG JIMMY'S SOCIAL MEDIA ACCOUNTS IN AND OUT
        // OF THE WORLD OF HOTLINE MIAMI AND NEVER I SAY NEVER HAS BIG JIMMY'S MOUTH EVER BEEN FILLED
        // WITH THE OTHERWODLY ALIEN TRASH THAT IS HAMBURGER, NEVER I TELL YOU, I HONESTLY,'
        // TRULY SAY THESE WORDS WITH ABSOLUTE SINCERE CERTAINTY OF THE HIGHEST MOST DIVINE PROPORTIONS
        // I HATE EVERYONE IN THIS FORUM YOU ALL ARE A DISCRASE TO THE BIG JIMMY FANDOM
        // ALL YOUR "BOO HOO LITTLE BIG JIMMY EATING HIS BURGER AGAIN" HE NEVER EVEN EATS BURGERS TO BEGIN WITH
        // YOU PREPUBECENT SWINE I SHALL DEVOUR YOUR ENTIRE HOUSE LIKE ITS STRAIGHT OUT OF BALKAN FOLKORE
        // NEVER NEVER NEVER NEVER POST ABOUT MY HUSBANDO BIG JIMMY AGAIN AND LEAVE THE FLIPPING HOTLINE MIAMI FANDOM
        // THANK YOU
        // - BIGJIMMYISLIFE43OFFICIAL
        // thanks for the 230 updoots! =D

        // val q = Query[Int, None](realm).addFilter()
        realm.query[Query1#Search, Query1#Filter, Query1]()
        realm.query[Query2#Search, Query2#Filter, Query2]()
    }
}
