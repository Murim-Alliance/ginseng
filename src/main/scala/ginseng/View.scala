package ginseng

trait View[T] {
    type View;
}

class Read[T] extends View[T] { type View = T }

class Write[T] extends View[T] { type View = T }

sealed trait BoolLogic {
    val teacherId: TeacherId
}

case class Not(override val teacherId: TeacherId) extends BoolLogic
case class And(override val teacherId: TeacherId) extends BoolLogic

trait Moogli[T] {
    def create(teacherId: TeacherId): BoolLogic
}

object Moogli:
    given Moogli[Without[_]] with {
        override def create(teacherId: TeacherId): BoolLogic = Not(teacherId)
    }
    given Moogli[With[_]] with {
        override def create(teacherId: TeacherId): BoolLogic = And(teacherId)
    }

sealed trait Filter {}

class Without[T] extends View[T] with Filter {
    type View = T

}

class With[T] extends View[T] with Filter {
    type View = T

}

trait FilterEncoder[T] {
    def encodeFilter(vector: Vector[Option[TeacherId]]): Vector[Option[BoolLogic]]
}

object FilterEncoder {
    given FilterEncoder[EmptyTuple] with
        final override def encodeFilter(vector: Vector[Option[TeacherId]]): Vector[Option[BoolLogic]] = Vector.empty

    given [H <: Filter: Moogli, T <: Tuple: FilterEncoder]: FilterEncoder[H *: T] with {
        final override def encodeFilter(vector: Vector[Option[TeacherId]]): Vector[Option[BoolLogic]] =
            Vector(vector(0).map(summon[Moogli[H]].create)) ++ summon[FilterEncoder[T]].encodeFilter(vector.drop(1))
    }
}
