package ginseng

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
 * This is done through the use of Scrolls, which can be taught to Disciples through the Patriarch.
 * The Alliance is responsible for allocating and deallocating Disciples.
 * The Alliance also stores the location of each Disciple in a Sect and the Sect it belongs to.
 */
class Realm {

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
    def query[S <: Tuple: TypeListEncoder, F <: Tuple: TypeListEncoder, Q <: QueryImpl[S, F]](): Unit = {
        // find the tables that contain all R but filter on F
        // (A, B: Hlist) => (BitSet, B: Hlist) => List[BitSet] => BitSet (reduce/fold it)
        // now we know which tables we have to get
        // get the list of the tables,
        // Consider List[R'] where R' is R prime
        // R' is a List for which the element is a tuple of which each element is a list of the type in tuple R
        // Now just provide a way to iterate over it or something

        val discipleList = summon[TypeListEncoder[S]].encodeTypeList(registry)

        val initialLength  = discipleList.length
        val mappedDisciple = discipleList.flatten
        if initialLength != mappedDisciple.length then return
        else ()
        discipleList.foreach(println(_))

        // TODO
    }

    /**
     * Teaches an Knowledge to a Disciple, by hiring a new Sifu to teach it.
     *
     * @param disciple  the Disciple to teach the Knowledge to.
     * @param knowledge the Knowledge to teach.
     * @tparam Type the type of the Knowledge to teach.
     */
    def teach[Type: ClassTag](disciple: Disciple, knowledge: Type): Unit = {
        // check if disciple is alive
        if !alliance.isRecruited(disciple) then return

        val sifuId = registry.getValue[Type] match {
            case Some(d1) => d1
            case None =>
                val d1 = recruit()
                registry.register[Type](d1)
                d1
        }

        patriarch.makeReceive(sifuId, disciple, knowledge, alliance.metas)
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
            val sifuId = registry.getValue[Type] match {
                case Some(d1) => d1
                case None     => return
            }

            patriarch.makeDismiss(sifuId, disciple, alliance.metas)
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
        val sifuId = registry.getValue[Type] match {
            case Some(scroll) => scroll
            case None         => return None
        }

        // Must be some
        val (sect, hall): (Sect, HallId) = alliance.metas(disciple.id).get
        sect.inquire(sifuId, hall).map(_.asInstanceOf[Type])
    }

    /**
     * Make a Sifu receive a Disciple.
     *
     * @param sifu     the Sifu to receive the Disciple.
     * @param disciple the Disciple to receive.
     */
    def receive(sifu: SifuId, disciple: Disciple): Unit = {

        // check if disciple is alive
        if !alliance.isRecruited(disciple) then ()
        else
            // Unit value because it's a tag
            patriarch.makeReceive(sifu, disciple, knowledge = (), alliance.metas)
    }

    /**
     * Graduates a Disciple from their Sifu's teachings.
     *
     * @param sifu     the Sifu to dismiss the Disciple.
     * @param disciple the Disciple dismiss
     */
    def dismiss(sifu: SifuId, disciple: Disciple): Unit = {
        if !alliance.isRecruited(disciple) then ()
        else {
            patriarch.makeDismiss(sifu, disciple, alliance.metas)
        }
    }

    /**
     * Inquires about a Sifu/Disciple relationship.
     *
     * @param sifu     the Sifu.
     * @param disciple the Disciple.
     * @return True if the Sifu is teaching the Disciple., False otherwise.
     */
    def inquire(sifu: SifuId, disciple: Disciple): Boolean = {
        if !alliance.isRecruited(disciple) then false
        else {
            // Must be some
            val (sect, hall): (Sect, HallId) = alliance.metas(disciple.id).get
            sect.inquire(sifu, hall).isDefined
        }
    }
}
