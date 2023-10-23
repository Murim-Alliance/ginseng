package ginseng

import scala.collection.mutable
import scala.compiletime.constValue
import scala.reflect.ClassTag


/**
 * A map that associates a value of type `V` with a class of type `K`.
 *
 * @tparam V the type of the value stored in the map.
 */
private[ginseng] final class TypeMap[V] {
	/**
	 * TODO
	 */
	private val inner = mutable.HashMap[ClassTag[_], V]();

	/**
	 * Registers a value of type `V` for the class `K`.
	 *
	 * @tparam K the type of the class to register the value for.
	 * @param value the value to register.
	 */
	def register[K](value: V)(implicit classTag: ClassTag[K]): Unit = this.inner.put(classTag, value)

	/**
	 * Returns the value associated with the class `K`, if any.
	 *
	 * @tparam K the type of the class to look up the value for.
	 * @return an `Option` containing the value associated with the class `K`, if such value exists.
	 */
	def getValue[K](implicit classTag: ClassTag[K]): Option[V] = this.inner.get(classTag)

	/**
	 * De-registers a type by the value it maps to
	 *
	 * @param value the value to de-register by.
	 */
	def DeregisterByValue(value: V): Unit = this.inner.filterInPlace((_, v1) => v1 == value)
}
