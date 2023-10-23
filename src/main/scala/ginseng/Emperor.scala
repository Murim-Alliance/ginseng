package ginseng

import scala.collection.{immutable, mutable}

/**
 * TODO
 */
object Emperor {
	/**
	 * TODO
	 *
	 * @return TODO
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
 * TODO
 *
 * @param sects     TODO
 * @param scrollIds TODO
 */
private[ginseng] class Emperor private
(
	private val sects: mutable.ArrayBuffer[Sect],
	private val scrollIds: mutable.ArrayBuffer[immutable.TreeSet[ScrollId]]
) {

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param metas    TODO
	 */
	def placeNewDisciple(disciple: Disciple, metas: mutable.ArrayBuffer[Option[(Sect, HallId)]]): Unit = {
		// Assumes disciple is valid and correct gen
		val sectReference = sects(0)
		val hall = sectReference.headCount()
		metas.update(disciple.id, Some((sectReference, hall)))

		// Add it in the first table which is guaranteed to exist and be only the disciple scroll
		sects(0).pushValue(Disciple.DiscipleScrollId, disciple)
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param scrollId TODO
	 * @param metas    TODO
	 */
	def forgetScrollForDisciple(disciple: Disciple, scrollId: ScrollId, metas: Metas): Unit = {
		// invariant, check before calling if disciple is active so this doesn't mess up
		// should be Some so don't check
		val (sect, hall): (Sect, HallId) = metas(disciple.id).get

		// now find where to transfer it to by getting the TreeSet[ScrollId], adding the given scrollId
		// and then search for it
		val nextSectScrolls = sect.exposeScrollIds.excl(scrollId)

		// check if the sect to transfer the disciple to already exists
		scrollIds.indexOf(nextSectScrolls) match {
			case -1 =>
				// create new sect
				val newSect = sect.reduceWith(scrollId)
				sects.addOne(newSect)
				scrollIds.addOne(nextSectScrolls)

				sect.lossyTransferHallTo(newSect, hall)
				metas.update(disciple.id, Some(newSect, 0))
			case i =>
				// know where to transfer it to
				val destinationSect = sects(i)
				if (sect ne destinationSect) {
					// transfer it and edit meta
					val newHall = sect.headCount()
					sect.lossyTransferHallTo(destinationSect, hall)
					metas.update(disciple.id, Some(destinationSect, newHall))
				}
				else {
					// don't do shit
				}
		}
	}

	/**
	 * TODO
	 *
	 * @param disciple TODO
	 * @param scroll   TODO
	 * @param scrollId TODO
	 * @param metas    TODO
	 */
	def teachScrollToDisciple(disciple: Disciple, scroll: Any, scrollId: ScrollId, metas: Metas): Unit = {
		// invariant, check before calling if disciple is active so this doesn't mess up
		// should be Some so don't check
		val (sect, hall): (Sect, HallId) = metas(disciple.id).get
		// now find where to transfer it to by getting the TreeSet[ScrollId], adding the given scrollId
		// and then search for it
		val nextSectScrolls = sect.exposeScrollIds.incl(scrollId)

		// check if the sect to transfer the disciple to already exists
		scrollIds.indexOf(nextSectScrolls) match {
			case -1 =>
				// sect doesn't exist yet, create the sect based on the sect we got from the disciple
				// extend the sect we know that the disciple is in with the scroll we want to add
				val newSect = sect.extendWith(scrollId)

				// now add it to the emperors knowledge
				sects.addOne(newSect)

				// also add the set of the scrolls so that we can find it in the future
				scrollIds.addOne(nextSectScrolls)


				// transfer disciple to the new sect, from the hall of the current sect
				sect.lossyTransferHallTo(newSect, hall)

				// now ensure the added scroll is there, push value will push into
				// the array of the newSect so it doesn't insert
				// Since newSect should have no elements and also have no gaps,
				// we know that the we can just push it onto the array
				newSect.pushValue(scrollId, scroll)

				// edit meta
				// we know the hall/row = 0 because the sect is unpopulated since we just made it
				metas.update(disciple.id, Some((newSect, 0)))
			case i =>
				// sect exists, add disciple to it and edit the meta
				val destinationSect = sects(i)

				// first check if  src != dst
				if (sect ne destinationSect) {
					// it's a different sect so do the transfer

					// note down the row it will be in which is the length since there are no gaps
					// arrays are 0-indexed so if we get headcount before transfer it is the corresponding
					// index of where it is transferred to
					val newHall = destinationSect.headCount()

					// transfer it to the destination which is an already existing sect
					sect.lossyTransferHallTo(destinationSect, hall)

					// since the transfer only transfers value that were in the old sect
					// we need to provide the value that destination won't have
					// there we just push it and match it by the scrollId
					// since we already confirmed the destination has the Scroll/ScrollId
					// this should *never* fail
					destinationSect.pushValue(scrollId, scroll)

					// edit the meta
					metas.update(disciple.id, Some((destinationSect, newHall)))
				} else {
					// just update the value
					sect.replaceScrollValue(scrollId, scroll, hall)
					// don't need to update the meta since we only updated the value an not the row/hall or sect/table
				}
		}
	}
}
