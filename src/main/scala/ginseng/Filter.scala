package ginseng
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

    given [H <: Filter: ModifierMapper, T <: Tuple: FilterEncoder]: FilterEncoder[H *: T] with {
        final override def encodeFilter(vector: Vector[Option[TeacherId]]): Vector[Option[BoolLogic]] =
            Vector(vector(0).map(summon[ModifierMapper[H]].create)) ++ summon[FilterEncoder[T]].encodeFilter(
              vector.drop(1)
            )
    }
}
