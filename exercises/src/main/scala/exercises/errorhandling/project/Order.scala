package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus._

import java.time.{Duration, Instant}

case class OrderID(value: String)
case class ItemID(value: String)
case class Order(
  id: OrderID,
  status: OrderStatus,
  createdAt: Instant
) {

  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): Either[OrderError, Order] =
    addItems(NEL(item))

  def addItems(items: NEL[Item]): Either[OrderError, Order] =
    status match {
      case draft: Draft       => Right(copy(status = Draft(draft.basket ++ items.toList)))
      case checkout: Checkout => Right(copy(status = Draft(checkout.basket.toList ++ items.toList)))
      case _                  => Left(InvalidStatus(status))
    }

  // 1. Implement `checkout` which attempts to move the `Order` to "Checkout" status.
  // `checkout` requires the order to be in the "Draft" status, otherwise it returns an `InvalidStatus` error.
  // `checkout` requires the order to contain at least one item, otherwise it returns an `EmptyBasket` error.
  def checkout: Either[OrderError, Order] =
    status match {
      case draft: Draft =>
        NEL.fromList(draft.basket) match {
          case None      => Left(EmptyBasket)
          case Some(nel) => Right(copy(status = Checkout(nel, None)))
        }
      case _ => Left(OrderError.InvalidStatus(status))
    }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case checkout: Checkout =>
        val newStatus = checkout.copy(deliveryAddress = Some(address))
        Right(copy(status = newStatus))
      case _ => Left(InvalidStatus(status))
    }

  // 2. Implement `submit` which attempts to move the `Order` to "Submitted" status.
  // `submit` requires the order to be in the "Checkout" status and to have a delivery address.
  // If `submit` succeeds, the resulting order must be in "Submitted" status and
  // have the field `submittedAt` defined.
  // Note: You may need to extend `OrderError`
  def submit(now: Instant): Either[OrderError, Order] =
    status match {
      case checkout: Checkout =>
        checkout.deliveryAddress match {
          case None          => Left(OrderError.NoAddressAvailable)
          case Some(address) => Right(copy(status = Submitted(checkout.basket, address, submittedAt = now)))
        }
      case _ => Left(OrderError.InvalidStatus(status))
    }

  // 3. Implement `deliver` which attempts to move the `Order` to "Delivered" status.
  // `deliver` requires the order to be in the "Submitted" status.
  // If `deliver` succeeds, the resulting order must be in "Delivered" status and
  // have the field `deliveredAt` defined.
  // If `deliver` succeeds, it also returns the time it took to deliver the order (duration
  // between `submittedAt` and `deliveredAt`).
  // Note: You may need to extend `OrderError`
  def deliver(now: Instant): Either[OrderError, Order] =
    status match {
      case submitted: Submitted =>
        Right(
          copy(
            status = Delivered(submitted.basket, submitted.deliveryAddress, submitted.submittedAt, deliveredAt = now)
          )
        )
      case _ => Left(OrderError.InvalidStatus(status))
    }
}

object Order {
  // Creates an empty draft order.
  def empty(id: OrderID, now: Instant): Order =
    Order(
      id = id,
      status = Draft(Nil),
      createdAt = now
    )
}
