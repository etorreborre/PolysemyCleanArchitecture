module UseCases.Reservations where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time.Calendar
import qualified Domain.ReservationDomain as Dom
  ( Reservation (..),
    ReservationMap (),
    addReservation,
    availableSeats,
    cancelReservation,
    isReservationPossible,
  )
import Numeric.Natural
import UseCases.Store
import UseCases.Tracer

data Reservations m = Reservations
  { availableSeats :: Day -> m Natural,
    tryReservation :: Dom.Reservation -> m (Either ReservationError ()),
    fetch :: Day -> m [Dom.Reservation],
    cancel :: Dom.Reservation -> m (),
    listAll :: m Dom.ReservationMap
  }

-- | The functional error, raised if a reservation is not possible
newtype ReservationError = ReservationNotPossible String deriving (Show, Eq)

-- | Create a new Reservations component providing all the required functionality for reserving seats at a restaurant
--   This component uses a Tracer for logging and a Store for persistence
newReservations :: forall m. Monad m => Tracer m -> Store Day [Dom.Reservation] m -> Reservations m
newReservations tracer store = Reservations {..}
  where
    availableSeats :: Day -> m Natural
    availableSeats day = do
      trace tracer $ "compute available seats for " ++ show day
      todaysReservations <- fetch day
      return $ Dom.availableSeats maxCapacity todaysReservations

    tryReservation :: Dom.Reservation -> m (Either ReservationError ())
    tryReservation res@(Dom.Reservation date _ _ requestedQuantity) = do
      trace tracer $ "trying to reserve " ++ show requestedQuantity ++ " more seats on " ++ show date
      todaysReservations <- fromMaybe [] <$> getValue store date
      let available = Dom.availableSeats maxCapacity todaysReservations
      if Dom.isReservationPossible res todaysReservations maxCapacity
        then Right <$> persistReservation res
        else pure . Left $ ReservationNotPossible ("Sorry, only " ++ show available ++ " seats left on " ++ show date)
      where
        persistReservation :: Dom.Reservation -> m ()
        persistReservation r@(Dom.Reservation day _ _ _) = do
          trace tracer $ "enter a new reservation to KV store: " ++ show r
          rs <- fetch day
          let updated = Dom.addReservation r rs
          trace tracer $ "storing: " ++ show updated
          insertKeyValue store day updated

    fetch :: Day -> m [Dom.Reservation]
    fetch day = do
      trace tracer $ "fetch reservations for " ++ show day
      maybeList <- getValue store day
      return $ fromMaybe [] maybeList

    cancel :: Dom.Reservation -> m ()
    cancel res@(Dom.Reservation date _ _ _) = do
      trace tracer $ "deleting reservation " ++ show res
      reservations <- fetch date
      trace tracer $ "before: " ++ show reservations
      let after = Dom.cancelReservation res reservations
      trace tracer $ "after: " ++ show after
      insertKeyValue store date after

    listAll :: m Dom.ReservationMap
    listAll = do
      trace tracer "listing all reservation entries"
      M.fromList <$> listValues store

-- | the maximum capacity of the restaurant.
-- | to keep things simple this just a constant value of 20.
-- | In real life this would kept persistent in a database, and would be accessed by yet another abstract effect.
maxCapacity :: Natural
maxCapacity = 20
