package ginseng

import ginseng.Entry.{Exiled, Recruited}

import scala.collection.mutable

/**
 * Represents the state of an Disciple in the Palace.
 * Disciple can either be Recruited, meaning it is currently allocated.
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
	/**
	 * Creates a new Palace with the Entity(0, 0) reserved.
	 *
	 * @return
	 */
	def apply(): Palace = {
		val allocator = new Palace()
		allocator.recruit()
		allocator
	}
}

/**
 * Type alias for Metas, which is an ArrayBuffer which optionally contains a Sect and HallId for a specific index.
 */
type Metas = mutable.ArrayBuffer[Option[(Sect, HallId)]]

/**
 * Manages Disciples and their data.
 */
private[ginseng] class Palace private {

	/**
	 * Stores which Disciples are alive and their generation.
	 */
	private val entries = mutable.ArrayBuffer[Entry]()

	/**
	 * Stores the indices of the Disciples are currently not allocated.
	 */
	private val exiled = mutable.ArrayBuffer[Int]()

	/**
	 * Stores the location of each Disciple in a Sect and the Sect it belongs to.
	 * If an Disciple is not currently allocated, the value in here will be None.
	 */
	val metas: Metas = mutable.ArrayBuffer[Option[(Sect, HallId)]]()



	/**
	 * Allocates a new Disciple or reuses an old Disciple if one is available.
	 * If the old Disciple is reused, its generation number is incremented.
	 *
	 * @return The allocated or reused Disciple.
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
				this.metas.addOne(None)
				entity
	}

	/**
	 * Deallocates the given Disciple if it is currently allocated.
	 * If the Disciple is deallocated, its corresponding entry in the `metas` array is set to None.
	 *
	 * @param disciple The Disciple to deallocate.
	 * @return True if the Disciple was deallocated, false otherwise.
	 */
	def exile(disciple: Disciple): Boolean = {
		if this.isActive(disciple) then
			this.entries.update(disciple.id, Entry.Exiled(disciple.gen + 1))
			this.exiled.addOne(disciple.id)
			this.metas.update(disciple.id, None)
			true
		else
			false
	}

	/**
	 * Determines whether the given Disciple is currently allocated and has the correct generation number.
	 *
	 * @param disciple The Disciple to check.
	 * @return True if the Disciple is currently allocated and has the correct generation number, false otherwise.
	 */
	private def isActive(disciple: Disciple): Boolean = {
		this.entries(disciple.id) match {
			case Entry.Recruited(gen) => gen == disciple.gen
			case Entry.Exiled(_) => false
		}
	}
}
