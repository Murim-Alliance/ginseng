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

trait ModifierMapper[T] {
    def create(teacherId: TeacherId): BoolLogic
}

object ModifierMapper:
    given [S]: ModifierMapper[Without[S]] with {
        override def create(teacherId: TeacherId): BoolLogic = Not(teacherId)
    }
    given [S]: ModifierMapper[With[S]] with {
        override def create(teacherId: TeacherId): BoolLogic = And(teacherId)
    }
