package ginseng

import ginseng.Entry.{Exiled, Recruited}

import scala.collection.mutable

/**
 * Represents the state of an Disciple in the Palace.
 *  Disciple can either be Recruited, meaning it is currently allocated.
 * If the Disciple is Exiled, the Disciple is currently not allocated, containing its next generation.
 */
private enum Entry {
  case Recruited(gen: Int)
  case Exiled(next_gen: Int)
}

/**
 * Responsible for allocating and deallocating Disciples.
 * It keeps track of which Disciples are active and their generation number.
 * It also stores the location of each Disciple in a Sect and the Sect it belongs to.
 * If an Disciple is not currently allocated, the value in here will be None.
 */
object Palace {
  def apply(): Palace = {
    val allocator = new Palace()
    // Reserve Entity(0, 0) for the Component Id of the Entity Component
    allocator.recruit()
    allocator
  }
}

type Hall = Int

class Palace private {
  /**
   * Type Alias for readability.
   */


  /**
   * Stores which Disciples are alive and their generation.
   */
  private val entries = mutable.ArrayBuffer[Entry]()

  /**
   * Stores the indices of the Disciples are currently not allocated.
   */
  private val exiled = mutable.ArrayBuffer[Int]()

  /**
   * Stores the location of each entity in a table and the table it belongs to.
   * If an entity is not currently allocated, the value in here will be None.
   */
  private val meta = mutable.ArrayBuffer[Option[(Sect, Hall)]]()

  /**
   * Allocates a new entity or reuses an old entity if one is available.
   * If the old entity is reused, its generation number is incremented.
   *
   * @return The allocated or reused entity.
   */
  def recruit(): Disciple = {
    this.exiled.lift(exiled.size - 1).map(exiled.remove) match
      case Some(value) =>
        // Reuse an old entity
        val next_gen = this.entries(value) match {
          case Entry.Recruited(_) => ???
          case Entry.Exiled(next_gen) => next_gen

        }
        this.entries.update(value, Entry.Recruited(next_gen))
        Disciple(value, next_gen)
      case None =>
        // Create a new entity
        val entry = Entry.Recruited(0)
        val entity = Disciple(this.entries.length, 0)
        this.entries.addOne(entry)
        this.meta.addOne(None)
        entity
  }

  /**
   * Deallocates the given entity if it is currently allocated.
   * If the entity is deallocated, its corresponding entry in the `meta` array is set to None.
   *
   * @param entity The entity to deallocate.
   * @return True if the entity was deallocated, false otherwise.
   */
  def exile(entity: Disciple): Boolean = {
    if this.isActive(entity) then
      this.entries.update(entity.id, Entry.Exiled(entity.gen + 1))
      this.exiled.addOne(entity.id)
      this.meta.update(entity.id, None)
      true
    else
      false
  }

  /**
   * Determines whether the given entity is currently allocated and has the correct generation number.
   *
   * @param entity The entity to check.
   * @return True if the entity is currently allocated and has the correct generation number, false otherwise.
   */
  private def isActive(entity: Disciple): Boolean = {
    this.entries(entity.id) match {
      case Entry.Recruited(gen) => gen == entity.gen
      case Entry.Exiled(_) => false
    }
  }
}
