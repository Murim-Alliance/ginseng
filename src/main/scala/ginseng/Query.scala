package ginseng

object Query {
	def apply[R <: NonEmptyTuple, F <: Tuple]() = new Query[R, F]()

}

class Query[R <: NonEmptyTuple, F <: Tuple] {

}
