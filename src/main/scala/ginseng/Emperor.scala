package ginseng

import scala.collection.immutable
import scala.collection.mutable

object Emperor {

    /**
     * Creates a new Emperor with the Disciple(0, 0) reserved.
     * This Disciple is the "Disciple Scroll" that is used to represent Disciples that don't have any Scrolls.
     *
     * @return a new instance of Emperor with the Disciple(0, 0) reserved.
     */
    def apply(): Emperor = {
        val sects = mutable.ArrayBuffer[Sect]()
        val sect1 = Sect().extendWith(Disciple(0, 0))
        sects.addOne(sect1)
        sect1.pushValue(Disciple.DiscipleScrollId, Disciple.DiscipleScrollId)
        val scrolls = mutable.ArrayBuffer[immutable.TreeSet[ScrollId]]()
        scrolls.addOne(sect1.exposeScrollIds)

        new Emperor(sects, scrolls)
    }
}

/**
 * A Emperor defines functionality to interact with collections of Sects.
 * The Emperor has the power to assign new Disciples, teach Scrolls to Disciples, and retire Scrolls from Disciples.
 *
 * @param sects     the Sects the Emperor manages.
 * @param scrollIds the ScrollIds of the Sects the Emperor manages.
 */
private[ginseng] class Emperor private (
    private val sects: mutable.ArrayBuffer[Sect],
    private val scrollIds: mutable.ArrayBuffer[immutable.TreeSet[ScrollId]]
) {

    /**
     * Assigns a new Disciple to be managed by the Emperor.
     * The Disciple will first be assigned to the Sect(0), under the "Disciple Scroll".
     *
     * NOTE: Assumes that the Disciple is valid and has the correct generation.
     * NOTE: Assumes that the first Sect exists and teaches only the "Disciple Scroll".
     *
     * @param disciple
     * @param metas
     */
    def assignNew(disciple: Disciple, metas: mutable.ArrayBuffer[Option[(Sect, HallId)]]): Unit = {
        val sectReference = sects(0)
        val hall          = sectReference.headCount()
        metas.update(disciple.id, Some((sectReference, hall)))

        sects(0).pushValue(Disciple.DiscipleScrollId, disciple)
    }

    /**
     * Retires a Scroll from a Disciple, meaning that the Disciple will no longer be taught the Scroll.
     *
     * NOTE: Assumes that the Disciple is active.
     *
     * @param disciple
     * @param scrollId
     * @param metas
     */
    def retireScroll(disciple: Disciple, scrollId: ScrollId, metas: Metas): Unit = {
        val (sect, hall): (Sect, HallId) = metas(disciple.id).get

        // Obtain the Scrolls that should be taught to the Disciple after the Scroll is retired.
        val nextSectScrolls = sect.exposeScrollIds.excl(scrollId)

        // Check if the Sect to transfer the Disciple to already exists.
        scrollIds.indexOf(nextSectScrolls) match {
            case -1 =>
                // Sect doesn't exist yet, create the Sect based on the Sect we got from the Disciple.
                val newSect = sect.reduceWith(scrollId)
                sects.addOne(newSect)
                scrollIds.addOne(nextSectScrolls)

                // Transfer the Disciple to the new Sect.
                sect.lossyTransferHallTo(newSect, hall)
                metas.update(disciple.id, Some(newSect, 0))
            case i =>
                // Sect exists, transfer Disciple to it and edit the meta.
                val destinationSect = sects(i)
                if (sect ne destinationSect) {
                    // It's a different Sect, so do the transfer.
                    val newHall = sect.headCount()
                    sect.lossyTransferHallTo(destinationSect, hall)
                    metas.update(disciple.id, Some(destinationSect, newHall))
                } else {
                    // Don't do anything, since the Disciple is already in the Sect.
                }
        }
    }

    /**
     * Teaches a Scroll to a Disciple, meaning that the Disciple will be taught the Scroll.
     *
     * NOTE: Assumes that the Disciple is active.
     *
     * @param disciple  the Disciple to teach the Scroll to.
     * @param scroll    the Scroll to teach to the Disciple.
     * @param scrollId  the ScrollId of the Scroll to teach to the Disciple.
     * @param metas     the metadata of the Disciples.
     */
    def teachScroll(disciple: Disciple, scroll: Any, scrollId: ScrollId, metas: Metas): Unit = {
        val (sect, hall): (Sect, HallId) = metas(disciple.id).get

        // Obtain the Scrolls that should be taught to the Disciple after the Scroll is taught.
        val nextSectScrolls = sect.exposeScrollIds.incl(scrollId)

        // Check if the Sect to transfer the Disciple to already exists.
        scrollIds.indexOf(nextSectScrolls) match {
            case -1 =>
                // Sect doesn't exist yet, create the Sect based on the Sect we got from the Disciple.
                val newSect = sect.extendWith(scrollId)
                sects.addOne(newSect)
                scrollIds.addOne(nextSectScrolls)

                // Transfer the Disciple to the new Sect.
                sect.lossyTransferHallTo(newSect, hall)

                // Since newSect should have no elements and also have no gaps, we know that the we can just push it onto the array.
                newSect.pushValue(scrollId, scroll)

                // We know that the Hall must be 0 because the Sect is unpopulated since we just made it.
                metas.update(disciple.id, Some((newSect, 0)))
            case i =>
                // Sect exists, transfer Disciple to it and edit the meta.
                val destinationSect = sects(i)

                // Check if the Sect is the same as the Sect the Disciple is currently in.
                if (sect ne destinationSect) {
                    // It's a different Sect, so do the transfer.

                    // Because we know there are no gaps, we know that the headCount will be the index of the Hall the Disciple is transferred to.
                    // NOTE: ArrayBuffers are 0-indexed, so we don't need to subtract 1.
                    val newHall = destinationSect.headCount()

                    // Transfer the Disciple to the new Sect.
                    sect.lossyTransferHallTo(destinationSect, hall)

                    // Push the Scroll onto the new Sect.
                    // We only have to provide the value that the destination Sect doesn't have,
                    // since we know the destination has transferred all the other Scroll values.
                    // NOTE: This should never fail, since we already confirmed that the destination Sect has the Scroll/ScrollId.
                    destinationSect.pushValue(scrollId, scroll)

                    // Update the meta to reflect the new Sect and Hall.
                    metas.update(disciple.id, Some((destinationSect, newHall)))
                } else {
                    // It's the same Sect, so just update the value.
                    sect.replaceScrollValue(scrollId, scroll, hall)

                    // We don't need to update the meta since we only updated the value and not the Hall or Sect.
                }
        }
    }
}
