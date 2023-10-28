package ginseng

import scala.collection.immutable
import scala.language.postfixOps
import scala.reflect.ClassTag

/**
 * TODO
 *
 * @tparam Search TODO
 * @tparam Filter TODO
 */
type Query    = [Search, Filter] =>> QueryImpl[MakeTuple[Search], MakeTuple[Filter]]
type NoFilter = With[Disciple]

/**
 * TODO
 *
 * @tparam T TODO
 */
type MakeTuple[T] = T match {
    case Tuple => T
    case _     => Tuple1[T]
}

/**
 * TODO
 *
 * @param extraFilter TODO
 * @tparam S TODO
 * @tparam F TODO
 */
class QueryImpl[S, F] private (private val realm: Realm, var extraFilters: Vector[BoolLogic] = Vector())
    extends Iterable[S] {
    type RealmSearch = S
    type RealmFilter = F

    def setFilters(filters: Vector[BoolLogic]): Unit = this.extraFilters = filters;

    override def iterator: Iterator[S] = ???
}

/**
 * TODO
 *
 * @tparam T TODO
 */
trait TypeListEncoder[T] {

    /**
     * TODO
     *
     * @param registry TODO
     * @return TODO
     */
    def encodeTypeList(registry: TypeMap[TeacherId]): Vector[Option[TeacherId]]
}

/**
 * TODO
 */
object TypeListEncoder {

    /**
     * TODO: Base case
     */
    given TypeListEncoder[EmptyTuple] with {
        final def encodeTypeList(registry: TypeMap[TeacherId]): Vector[Option[TeacherId]] =
            Vector.empty[Option[TeacherId]]
    }

    /**
     * TODO: Inductive case
     */
    given [S: ClassTag, H[_] <: View[_], T <: Tuple: TypeListEncoder]: TypeListEncoder[H[S] *: T] with {
        final def encodeTypeList(registry: TypeMap[TeacherId]): Vector[Option[TeacherId]] = {
            Vector(registry.getValue[S]) ++ summon[TypeListEncoder[T]].encodeTypeList(registry)
        }
    }
}
