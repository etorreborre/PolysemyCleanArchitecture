{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module InterfaceAdapters.ReservationRestService where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map as M
import Data.Time.Calendar (Day)
import qualified Domain.ReservationDomain as Dom (Reservation, ReservationMap)
import Numeric.Natural
import Servant
import UseCases.Reservations (ReservationError (..), Reservations)
import qualified UseCases.Reservations as Reservations

-- | Declaring the routes of the REST API for Restaurant Reservations
type ReservationAPI =
  "reservations" :> Summary "retrieve a map of all reservations (Day -> [Reservation])"
    :> Get '[JSON] Dom.ReservationMap -- GET    /reservations
    :<|> "reservations" :> Summary "retrieve list of reservations for a given day"
      :> Capture' '[Description "YYYY-MM-DD"] "day" Day
      :> Get '[JSON] [Dom.Reservation] -- GET    /reservations/YYYY-MM-DD
    :<|> "reservations" :> Summary "place a new reservation"
      :> ReqBody '[JSON] Dom.Reservation
      :> Post '[JSON] () -- POST   /reservations
    :<|> "reservations" :> Summary "cancel a reservation"
      :> ReqBody '[JSON] Dom.Reservation
      :> Delete '[JSON] () -- DELETE /reservations
    :<|> "seats" :> Summary "retrieve number of free seats for a given day"
      :> Capture' '[Description "YYYY-MM-DD"] "day" Day
      :> Get '[JSON] Natural -- GET    /seats/YYYY-MM-DD

-- | This Haskell data type maps exactly to the Servant types used in the definitions above
data ReservationApi = ReservationApi
  { availableSeats :: Day -> Handler Natural,
    tryReservation :: Dom.Reservation -> Handler (),
    fetch :: Day -> Handler [Dom.Reservation],
    cancel :: Dom.Reservation -> Handler (),
    listAll :: Handler Dom.ReservationMap
  }

-- | Create a ReservationApi component translating the domain values and errors
--   to the values and errors used in the REST API
newReservationApi :: Reservations IO -> ReservationApi
newReservationApi rs = ReservationApi {..}
  where
    availableSeats :: Day -> Handler Natural
    availableSeats = liftIO . Reservations.availableSeats rs

    tryReservation :: Dom.Reservation -> Handler ()
    tryReservation r = Handler . ExceptT $ handleErrors <$> Reservations.tryReservation rs r

    fetch :: Day -> Handler [Dom.Reservation]
    fetch = liftIO . Reservations.fetch rs

    cancel :: Dom.Reservation -> Handler ()
    cancel = liftIO . Reservations.cancel rs

    listAll :: Handler Dom.ReservationMap
    listAll = liftIO $ Reservations.listAll rs

-- | Create a Server from a list of Haskell functions
newReservationServer :: ReservationApi -> ServerT ReservationAPI Handler
newReservationServer ReservationApi {..} = do
  listAll -- GET    /reservations
    :<|> fetch -- GET    /reservations/YYYY-MM-DD
    :<|> tryReservation -- POST   /reservations
    :<|> cancel -- DELETE /reservations
    :<|> availableSeats -- GET    /seats/YYYY-MM-DD

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy

-- | Translate domain errors as HTTP Errors
handleErrors :: Either ReservationError b -> Either ServerError b
handleErrors (Left (ReservationNotPossible msg)) = Left err412 {errBody = pack msg}
handleErrors (Right value) = Right value
