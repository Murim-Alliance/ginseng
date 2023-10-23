package ginseng

import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * A map that associates a value of type `V` with a class of type `K`.
 *
 * @tparam V the type of the value stored in the map.
 */
class TypeMap[V] {
  private val inner = mutable.HashMap[Class[_], V]();

  /**
   * Registers a value of type `V` for the class `K`.
   *
   * @tparam K the type of the class to register the value for.
   * @param value the value to register.
   */
  def register[K: ClassTag](value: V): Unit = this.inner.put(implicitly[ClassTag[K]].runtimeClass, value)

  /**
   * Returns the value associated with the class `K`, if any.
   *
   * @tparam K the type of the class to look up the value for.
   * @return an `Option` containing the value associated with the class `K`, if such value exists.
   */
  def getValue[K: ClassTag]: Option[V] = this.inner.get(implicitly[ClassTag[K]].runtimeClass)

  /**
   * De-registers a type by the value it maps to
   *
   * @param value the value to de-register by.
   */
  def DeregisterByValue(value: V): Unit = this.inner.filterInPlace((_, v1) => v1 == value)

}
