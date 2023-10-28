package ginseng

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait TypeListCast[T] {

    def castCourtyard(courtyards: Vector[Courtyard]): Vector[ArrayBuffer[_]]
}

object TypeListCast:

    given TypeListCast[EmptyTuple] with {

        override def castCourtyard(courtyards: Vector[Courtyard]): Vector[ArrayBuffer[_]] = Vector.empty
    }

    given [H, T <: Tuple: TypeListCast]: TypeListCast[H *: T] with {
        override def castCourtyard(courtyards: Vector[Courtyard]): Vector[ArrayBuffer[_]] =
            Vector(courtyards(0).asInstanceOf[ArrayBuffer[H]]) ++ summon[TypeListCast[T]].castCourtyard(
              courtyards.drop(1)
            )
    }

end TypeListCast
