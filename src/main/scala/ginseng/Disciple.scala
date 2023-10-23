package ginseng

/**
 * Represents an entity with a unique identifier and generation number.
 *
 * @param id  The unique identifier of the entity.
 * @param gen The generation of the Entity, used to track the lifetime of the Entity
 *            When an Entity is deallocated, its generation is incremented, which invalidates any references to the Entity.
 *            When an Entity is allocated, its generation is set to 0, which means that the Entity is alive and can be used.
 */
case class Disciple(id: Int, gen: Int)
