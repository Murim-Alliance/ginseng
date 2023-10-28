package ginseng

import scala.reflect.ClassTag

object Realm {

    /**
     * Creates a new Realm with the Disciple(0, 0) reserved.
     *
     * @return a new instance of Realm with the Disciple(0, 0) reserved.
     */
    def apply(): Realm = new Realm()
}

/**
 * A Realm defines functionality to interact with collections of Disciples.
 * This is done through the use of Scrolls, which can be taught to Disciples through the Patriarch.
 * The Alliance is responsible for allocating and deallocating Disciples.
 * The Alliance also stores the location of each Disciple in a Sect and the Sect it belongs to.
 */
class Realm private {

    /**
     * Defines a unique Disciple for each type.
     */
    private val registry = TypeMap[Disciple]()

    /**
     * Instance of the Alliance of Sects.
     */
    private val alliance = Alliance()

    /**
     * Instance of the Patriarch of the Alliance.
     */
    private val patriarch = Patriarch()

    /**
     * Queries the Realm for all Disciples that have Scrolls of type R but not of type F.
     *
     * @tparam S TODO
     * @tparam F TODO
     * @tparam Q TODO
     * @return an Array of Scrolls of type R, but not of type F, which is indexed by DiscipleId.
     */
    def query[S <: Tuple: TypeListEncoder: TypeListCast, F <: Tuple: FilterEncoder: TypeListEncoder, Q <: QueryImpl[S, F]](): Unit = {
        // find the tables that contain all R but filter on F
        // (A, B: Hlist) => (BitSet, B: Hlist) => List[BitSet] => BitSet (reduce/fold it)
        // now we know which tables we have to get
        // get the list of the tables,
        // Consider List[R'] where R' is R prime
        // R' is a List for which the element is a tuple of which each element is a list of the type in tuple R
        // Now just provide a way to iterate over it or something

        val discipleList = summon[TypeListEncoder[S]].encodeTypeList(registry)

        val initialLength  = discipleList.length
        val flattenedDisciples = discipleList.flatten
        if initialLength != flattenedDisciples.length then return
        else ()

        val filterList = summon[TypeListEncoder[F]].encodeTypeList(registry)
        // For each element in the vector, wrap it in the appropriate filter
        val (andFilter, notFilter) = summon[FilterEncoder[F]].encodeFilter(filterList).flatten.partitionMap {
            case i: And => Left(i.teacherId)
            case i: Not => Right(i.teacherId)
        }
        if filterList.length - notFilter.length != andFilter.length then return
        else ()

        // combine the two AND filters
        val sectsFound = patriarch.findAllSectsWithFilter(flattenedDisciples ++ andFilter, notFilter);

        val r = flattenedDisciples.map(id => sectsFound.map(_.getCourtyard(id).get)).map(summon[TypeListCast[S]].castCourtyard(_))

        println(r)
        // get all the columns we need in this?
//        sectsFound.map(sect => {
//            sect.
//        })
    }

    /**
     * Teaches an Knowledge to a Disciple, by hiring a new Teacher to teach it.
     *
     * @param disciple  the Disciple to teach the Knowledge to.
     * @param knowledge the Knowledge to teach.
     * @tparam Type the type of the Knowledge to teach.
     */
    def teach[Type: ClassTag](disciple: Disciple, knowledge: Type): Unit = {
        // check if disciple is alive
        if !alliance.isRecruited(disciple) then return

        val teacherId = registry.getValue[Type] match {
            case Some(d1) => d1
            case None =>
                val d1 = recruit()
                registry.register[Type](d1)
                d1
        }

        patriarch.makeReceive(teacherId, disciple, knowledge, alliance.metas)
    }

    /**
     * Recruits a new Disciple into the Alliance.
     *
     * @return the new Disciple.
     */
    def recruit(): Disciple = {
        // Disciple(0, 0) is already hardcoded to be the disciple disciple
        val newDisciple = alliance.recruit()
        patriarch.subjugate(disciple = newDisciple, metas = alliance.metas)
        newDisciple
    }

    /**
     * Discontinues the teaching of a certain type of Knowledge to the Disciple.
     *
     * @param disciple the Disciple to discontinue teaching.
     * @tparam Type the type of the Knowledge to discontinue teaching.
     */
    def discontinue[Type: ClassTag](disciple: Disciple): Unit = {
        if !alliance.isRecruited(disciple) then ()
        else {
            val teacherId = registry.getValue[Type] match {
                case Some(d1) => d1
                case None     => return
            }

            patriarch.makeDismiss(teacherId, disciple, alliance.metas)
        }
    }

    /**
     * Inquires about the elemental Knowledge being taught to the given Disciple, if any.
     * An Element is any type that can be converted into a ClassTag.
     *
     * @param disciple the Disciple to inquire about.
     * @tparam Type the type of the Knowledge to inquire about.
     * @return an `Option` containing the Knowledge of type `Type` that is taught to the given Disciple, if any.
     */
    def inquire[Type: ClassTag](disciple: Disciple): Option[Type] = {
        if !alliance.isRecruited(disciple) then return None
        val teacherId = registry.getValue[Type] match {
            case Some(scroll) => scroll
            case None         => return None
        }

        // Must be some
        val (sect, hall): (Sect, HallId) = alliance.metas(disciple.id).get
        sect.inquire(teacherId, hall).map(_.asInstanceOf[Type])
    }

    /**
     * Make a Teacher receive a Disciple.
     *
     * @param teacher     the Teacher to receive the Disciple.
     * @param disciple the Disciple to receive.
     */
    def receive(teacher: Disciple, disciple: Disciple): Unit = {

        // check if disciple is alive
        if !alliance.isRecruited(disciple) then ()
        else
            // Unit value because it's a tag
            patriarch.makeReceive(teacher, disciple, knowledge = (), alliance.metas)
    }

    /**
     * Graduates a Disciple from their Teacher's teachings.
     *
     * @param teacher     the Teacher to dismiss the Disciple.
     * @param disciple the Disciple dismiss
     */
    def dismiss(teacher: Disciple, disciple: Disciple): Unit = {
        if !alliance.isRecruited(disciple) then ()
        else {
            patriarch.makeDismiss(teacher, disciple, alliance.metas)
        }
    }

    /**
     * Inquires about a Teacher/Disciple relationship.
     *
     * @param teacher     the Teacher.
     * @param disciple the Disciple.
     * @return True if the Teacher is teaching the Disciple., False otherwise.
     */
    def inquire(teacher: Disciple, disciple: Disciple): Boolean = {
        if !alliance.isRecruited(disciple) then false
        else {
            // Must be some
            val (sect, hall): (Sect, HallId) = alliance.metas(disciple.id).get
            sect.inquire(teacher, hall).isDefined
        }
    }
}
