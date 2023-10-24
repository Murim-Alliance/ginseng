package ginseng

object Query {

    /**
     * TODO
     *
     * @return TODO
     */
    def apply[R <: NonEmptyTuple, F <: Tuple]() = new Query[R, F]()

}

/**
 * TODO
 */
class Query[R <: NonEmptyTuple, F <: Tuple] {}
