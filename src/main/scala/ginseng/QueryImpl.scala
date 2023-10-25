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
type Query[Search, Filter] = QueryImpl[MakeTuple[Search], MakeTuple[Filter]]

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
 */
trait Filter {}

/**
 * TODO
 */
class Unfiltered extends Filter {}

/**
 * TODO
 *
 * @tparam T TODO
 */
class With[T] extends Filter

/**
 * TODO
 *
 * @tparam T TODO
 */
class Without[T] extends Filter

/**
 * TODO
 *
 * @param extraFilter TODO
 * @tparam S TODO
 * @tparam F TODO
 */
class QueryImpl[S, F](val extraFilter: Vector[With[Disciple] | Without[Disciple]] = Vector()) {
    type Search = S
    type Filter = F
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
    def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]]
}

/**
 * TODO
 */
object TypeListEncoder {

    /**
     * TODO: Base case
     */
    given TypeListEncoder[EmptyTuple] with {
        final def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]] = Vector.empty[Option[Disciple]]
    }

    /**
     * TODO: Inductive case
     */
    given [H: ClassTag, T <: Tuple: TypeListEncoder]: TypeListEncoder[H *: T] with {

        final def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]] =
            Vector(registry.getValue[H]) ++ summon[TypeListEncoder[T]].encodeTypeList(registry)
    }
}
