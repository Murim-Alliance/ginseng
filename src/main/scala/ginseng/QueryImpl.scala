package ginseng

import scala.collection.immutable
import scala.language.postfixOps
import scala.reflect.ClassTag

type Query[Search, Filter] = QueryImpl[MakeTuple[Search], MakeTuple[Filter]]

type MakeTuple[T] = T match {
	case Tuple => T
	case _ => Tuple1[T]
}

trait Filter {}

class Unfiltered extends Filter {}

class With[T] extends Filter

class Without[T] extends Filter

class QueryImpl[S, F] (val extraFilter: Vector[With[Disciple] | Without[Disciple]] = Vector()) {
	type Search = S
	type Filter = F
}

trait TypeListEncoder[T]  {
	def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]]
}

object TypeListEncoder {
	// Base case
	given TypeListEncoder[EmptyTuple] with {
		final def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]] = Vector.empty[Option[Disciple]]
	}


	// Inductive case
	given[H: ClassTag, T <: Tuple : TypeListEncoder]: TypeListEncoder[H *: T] with {

		final def encodeTypeList(registry: TypeMap[Disciple]): Vector[Option[Disciple]] =
			Vector(registry.getValue[H]) ++ summon[TypeListEncoder[T]].encodeTypeList(registry)
	}
}
