package ginseng

import scala.collection.{immutable, mutable}

object Patriarch {

    /**
     * Creates a new Patriarch with the Disciple(0, 0) reserved.
     * This Disciple is the Ancestor Sifu which teaches all Disciples.
     *
     * @return a new instance of Patriarch with the Disciple(0, 0) reserved.
     */
    def apply(): Patriarch = {
        val sects = mutable.ArrayBuffer[Sect]()
        val sect1 = Sect().hire(Disciple(0, 0))
        sects.addOne(sect1)
        sect1.induction(Disciple.AncestorSifuId, Disciple.AncestorSifuId)
        val sifus = mutable.ArrayBuffer[immutable.TreeSet[SifuId]]()
        sifus.addOne(sect1.sifus)

        new Patriarch(sects, sifus)
    }
}

/**
 * A Patriarch defines functionality to interact with collections of Sects.
 * The Patriarch has the power to subjugate Disciples and make Sifus receive and dismiss Disciples.
 *
 * @param sects     the Sects the Patriarch manages.
 * @param sifuIds   the Sifus of the Sects the Patriarch manages.
 */
private[ginseng] class Patriarch private (
    private val sects: mutable.ArrayBuffer[Sect],
    private val sifuIds: mutable.ArrayBuffer[immutable.TreeSet[SifuId]]
) {

    /**
     * Turn a Disciple into a subject of the Patriarch.
     * The Disciple will first be assigned to the Sect(0), under the Ancestor Sifu.
     *
     * NOTE: Assumes that the Disciple is valid and has the correct generation.
     * NOTE: Assumes that the first Sect exists and the Ancestor Sifu teaches there.
     *
     * @param disciple the disciple to subjugate.
     * @param metas    metadata.
     */
    def subjugate(disciple: Disciple, metas: mutable.ArrayBuffer[Option[(Sect, HallId)]]): Unit = {
        val sectReference = sects(0)
        val hall          = sectReference.headCount()

        metas.update(disciple.id, Some((sectReference, hall)))
        sects(0).induction(Disciple.AncestorSifuId, disciple)
    }

    /**
     * Make a Sifu dismiss a Disciple.
     *
     * NOTE: Assumes that the Disciple is active.
     *
     * @param sifu     the Sifu who should dismiss the Disciple.
     * @param disciple the Disciple to dismiss.
     * @param metas    metadata.
     */
    def makeDismiss(sifu: SifuId, disciple: Disciple, metas: Metas): Unit = {
        val (sect, hall): (Sect, HallId) = metas(disciple.id).get

        // Obtain the all the Sifus that are teaching at the Sect with the new Sifu included.
        val nextSectSifus = sect.sifus.excl(sifu)

        // Check if the Sect to transfer the Disciple to already exists.
        sifuIds.indexOf(nextSectSifus) match {
            case -1 =>
                // Sect doesn't exist yet, create the Sect based on the Sect we got from the Disciple.
                val newSect = sect.retire(sifu)
                sects.addOne(newSect)
                sifuIds.addOne(nextSectSifus)

                // Transfer the Disciple to the new Sect.
                sect.transferToOtherSect(hall, newSect)
                metas.update(disciple.id, Some(newSect, 0))
            case i =>
                // Sect exists, transfer Disciple to it and edit the meta.
                val destinationSect = sects(i)
                if (sect ne destinationSect) {
                    // It's a different Sect, so do the transfer.
                    val newHall = sect.headCount()
                    sect.transferToOtherSect(hall, destinationSect)
                    metas.update(disciple.id, Some(destinationSect, newHall))
                } else {
                    // Don't do anything, since the Disciple is already in the Sect.
                }
        }
    }

    /**
     * Make the Sifu receive a Disciple, teaching them the given Knowledge.
     *
     * NOTE: Assumes that the Disciple is active.
     *
     * @param sifu      the Sifu who should receive the Disciple.
     * @param disciple  the Disciple to receive.
     * @param knowledge the Knowledge to teach the Disciple.
     * @param metas     metadata.
     */
    def makeReceive(sifu: SifuId, disciple: Disciple, knowledge: Knowledge, metas: Metas): Unit = {
        val (sect, hall): (Sect, HallId) = metas(disciple.id).get

        // Obtain the all the Sifus that are teaching at the Sect with the new Sifu included.
        val nextSectSifus = sect.sifus.incl(sifu)

        // Check if the Sect to transfer the Disciple to already exists.
        sifuIds.indexOf(nextSectSifus) match {
            case -1 =>
                // Sect doesn't exist yet, create the Sect based on the Sect we got from the Disciple.
                val newSect = sect.hire(sifu)
                sects.addOne(newSect)
                sifuIds.addOne(nextSectSifus)

                // Transfer the Disciple to the new Sect.
                sect.transferToOtherSect(hall, newSect)

                // Since newSect should have no elements and also have no gaps, we know that the we can just push it onto the array.
                newSect.induction(sifu, knowledge)

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
                    sect.transferToOtherSect(hall, destinationSect)

                    // Push the Sifu onto the new Sect.
                    // We only have to provide the value that the destination Sect doesn't have,
                    // since we know the destination has transferred all the other Sifu values.
                    // NOTE: This should never fail, since we already confirmed that the destination Sect has the Sifu/SifuId.
                    destinationSect.induction(sifu, knowledge)

                    // Update the meta to reflect the new Sect and Hall.
                    metas.update(disciple.id, Some((destinationSect, newHall)))
                } else {
                    // It's the same Sect, so just update the value.
                    sect.tutor(sifu, knowledge, hall)

                    // We don't need to update the meta since we only updated the value and not the Hall or Sect.
                }
        }
    }
}
