module UseCases.Tracer where

-- | Simple interface for logging messages
newtype Tracer m = Tracer {trace :: String -> m ()}
