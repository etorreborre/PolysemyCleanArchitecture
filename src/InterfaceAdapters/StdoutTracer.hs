module InterfaceAdapters.StdoutTracer where

import InterfaceAdapters.Config
import UseCases.Tracer

-- | Return a tracer based on the verbose flag in the config
newTracer :: Config -> Tracer IO
newTracer config = if verbose config then newStdoutTracer else noTracer

-- | Implementation of a Tracer printing to the console
newStdoutTracer :: Tracer IO
newStdoutTracer = Tracer print

-- | Implementation of a Tracer doing nothing
noTracer :: Tracer IO
noTracer = Tracer (const (pure ()))
