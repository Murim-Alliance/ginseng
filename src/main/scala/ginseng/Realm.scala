package ginseng

import scala.collection.View
import scala.reflect.ClassTag

object Realm {

    /**
     * Creates a new Realm with the Disciple(0, 0) reserved.
     *
     * @return a new instance of Realm with the Disciple(0, 0) reserved.
     */
    def apply() = new Realm()
}

/**
 * A Realm defines functionality to interact with collections of Disciples.
 * This is done through the use of Scrolls, which can be taught to Disciples through the Emperor.
 * The Palace is responsible for allocating and deallocating Disciples.
 * The Palace also stores the location of each Disciple in a Sect and the Sect it belongs to.
 */
class Realm {

    /**
     * Defines a unique Disciple for each type.
     */
    private val registry = TypeMap[Disciple]()

    /**
     * Instance of the Palace.
     */
    private val palace = Palace()

    /**
     * Instance of the Emperor.
     */
    private val emperor = Emperor()

    /**
     * Queries the Realm for all Disciples that have Scrolls of type R but not of type F.
     *
     * @return an Array of Scrolls of type R, but not of type F, which is indexed by DiscipleId.
     */
    def query[R <: NonEmptyTuple, F <: Tuple](): Array[R] = {
        // find the tables that contain all R but filter on F
        // (A, B: Hlist) => (BitSet, B: Hlist) => List[BitSet] => BitSet (reduce/fold it)
        // now we know which tables we have to get
        // get the list of the tables,
        // Consider List[R'] where R' is R prime
        // R' is a List for which the element is a tuple of which each element is a list of the type in tuple R
        // Now just provide a way to iterate over it or something

        ???
    }

    /**
     * Teaches an Elemental Scroll to a Disciple.
     * An Element is any type that can be converted into a ClassTag.
     *
     * @param disciple the Disciple to teach the Scroll to.
     * @param value    the value of the Scroll to teach.
     * @tparam Element the type of the Scroll to teach.
     */
    def teachElementalScroll[Element: ClassTag](disciple: Disciple, value: Element): Unit = {
        // check if disciple is alive
        if !palace.isActive(disciple) then return

        val scrollId = registry.getValue[Element] match {
            case Some(d1) => d1
            case None =>
                val d1 = reincarnate()
                registry.register[Element](d1)
                d1
        }
        emperor.teachScroll(disciple, value, scrollId, palace.metas)
    }

    /**
     * Retires an Elemental Scroll from a Disciple.
     * An Element is any type that can be converted into a ClassTag.
     *
     * @param disciple the Disciple to retire the Scroll from.
     * @param scrollId the Scroll to retire.
     * @tparam Element the type of the Scroll to retire.
     */
    def retireElementalScroll[Element: ClassTag](disciple: Disciple, scrollId: ScrollId): Unit = {
        if !palace.isActive(disciple) then ()
        else {
            val scrollId = registry.getValue[Element] match {
                case Some(d1) => d1
                case None     => return
            }
            emperor.retireScroll(disciple, scrollId, palace.metas)
        }
    }

    /**
     * Returns the Elemental Scroll of type `Element` that is taught to the given Disciple, if any.
     * An Element is any type that can be converted into a ClassTag.
     *
     * @param disciple the Disciple to get the Scroll from.
     * @tparam Element the type of the Scroll to get.
     * @return an `Option` containing the Scroll of type `Element` that is taught to the given Disciple, if any.
     */
    def inquireElementalKnowledge[Element: ClassTag](disciple: Disciple): Option[Element] = {
        if !palace.isActive(disciple) then return None
        val scrollId = registry.getValue[Element] match {
            case Some(scroll) => scroll
            case None         => return None
        }
        // Must be some
        val (sect, hall): (Sect, HallId) = palace.metas(disciple.id).get
        sect.getScrollValue(scrollId, hall).map(_.asInstanceOf[Element])
    }

    /**
     * Teaches a Martial Scroll to a Disciple.
     * A Martial Scroll is a Scroll that is taught by another Disciple.
     * The Disciple that teaches the Scroll is called the Sifu.
     *
     * @param disciple the Disciple to teach the Scroll to.
     * @param sifu     the Disciple that teaches the Scroll.
     */
    def teachMartialScroll(disciple: Disciple, sifu: Disciple): Unit = {

        // check if disciple is alive
        if !palace.isActive(disciple) then return
        else {
            // Unit value because it's a tag
            emperor.teachScroll(disciple = disciple, scroll = (), scrollId = sifu, palace.metas)
        }
    }

    /**
     * Retires a Martial Scroll from a Disciple.
     * A Martial Scroll is a Scroll that is taught by another Disciple.
     * The Disciple that teaches the Scroll is called the Sifu.
     *
     * @param disciple the Disciple to retire the Scroll from.
     * @param sifu     the Disciple that teaches the Scroll.
     */
    def retireMartialScroll(disciple: Disciple, sifu: Disciple): Unit = {
        if !palace.isActive(disciple) then ()
        else {
            emperor.retireScroll(disciple, sifu, palace.metas)
        }
    }

    /**
     * Returns the Martial Scroll that is taught to the given Disciple, if any.
     * A Martial Scroll is a Scroll that is taught by another Disciple.
     * The Disciple that teaches the Scroll is called the Sifu.
     *
     * @param disciple the Disciple to get the Scroll from.
     * @param sifu     the Disciple that teaches the Scroll.
     * @return True if the Disciple has the Scroll, False otherwise.
     */
    def inquireMartialKnowledge(disciple: Disciple, sifu: Disciple): Boolean = {
        if !palace.isActive(disciple) then false
        else {
            // Must be some
            val (sect, hall): (Sect, HallId) = palace.metas(disciple.id).get
            sect.getScrollValue(sifu, hall).isDefined
        }
    }

    /**
     * Reincarnates a new Disciple into the Realm.
     *
     * @return the new Disciple.
     */
    def reincarnate(): Disciple = {
        // Disciple(0, 0) is already hardcoded to be the disciple disciple
        val newDisciple = palace.recruit()
        emperor.assignNew(disciple = newDisciple, metas = palace.metas)
        newDisciple
    }
}
