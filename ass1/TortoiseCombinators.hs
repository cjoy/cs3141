module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
-- Identiy cases
andThen Stop i2 = i2
andThen i1 Stop = i1
-- Base Cases
andThen (Move d Stop) i2 = Move d i2
andThen (Turn a Stop) i2 = Turn a i2
andThen (SetStyle l Stop) i2 = SetStyle l i2
andThen (SetColour c Stop) i2 = SetColour c i2
andThen (PenDown Stop) i2 = PenDown i2
andThen (PenUp Stop) i2 = PenUp i2
-- Recursive Cases
andThen (Move d i1) i2 = Move d (i1 `andThen` i2)
andThen (Turn a i1) i2 = Turn a (i1 `andThen` i2)
andThen (SetStyle l i1) i2 = SetStyle l (i1 `andThen` i2)
andThen (SetColour c i1) i2 = SetColour c (i1 `andThen` i2)
andThen (PenDown i1) i2 = PenDown (i1 `andThen` i2)
andThen (PenUp i1) i2 = PenUp (i1 `andThen` i2)

-----------------------------------------------------------------

loop :: Int -> Instructions -> Instructions
loop n i
  | n <= 0 = Stop
  | otherwise = i `andThen` (loop (n - 1) i)

-----------------------------------------------------------------

-- Somehow need to set the start state when PenUp
invisibly :: Instructions -> Instructions
invisibly (PenUp i) = invisiblyUtil (PenUp i) False
invisibly i = invisiblyUtil i True

-- invisiblyUtil - Recursive helper, in order to maintain penState
invisiblyUtil :: Instructions -> Bool -> Instructions
-- Base cases
invisiblyUtil Stop b = Stop
invisiblyUtil (Move d Stop) b = PenUp (Move d (terminatePenState b))
invisiblyUtil (Turn a Stop) b = Turn a (terminatePenState b)
invisiblyUtil (SetStyle l Stop) b = SetStyle l (terminatePenState b)
invisiblyUtil (SetColour c Stop) b = SetColour c (terminatePenState b)
invisiblyUtil (PenDown Stop) b = PenDown Stop
invisiblyUtil (PenUp Stop) b = PenUp Stop
-- Recursive cases
invisiblyUtil (Move d i) b = PenUp (Move d (invisiblyUtil i b))
invisiblyUtil (Turn a i) b = Turn a (invisiblyUtil i b)
invisiblyUtil (SetStyle l i) b = SetStyle l (invisiblyUtil i b)
invisiblyUtil (SetColour c i) b = SetColour c (invisiblyUtil i b)
invisiblyUtil (PenDown i) b = PenUp (invisiblyUtil i True)
invisiblyUtil (PenUp i) b = PenUp (invisiblyUtil i False)

-- Determine the last pen state
terminatePenState :: Bool -> Instructions
terminatePenState b
  | b == True = PenDown Stop
  | otherwise = PenUp Stop

-----------------------------------------------------------------

retrace :: Instructions -> Instructions
retrace (PenUp i) = retraceUtil i (PenDown Stop) (Solid 1) white False
retrace i = retraceUtil i (PenDown Stop) (Solid 1) white True

retraceUtil :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
-- Base Cases
retraceUtil Stop acc pStyle pColour pPen = acc
retraceUtil (Move d Stop) acc pStyle pColour pPen = Move (negate d) acc
retraceUtil (Turn a Stop) acc pStyle pColour pPen = Turn (negate a) acc
retraceUtil (SetStyle l Stop) acc pStyle pColour pPen = SetStyle pStyle acc
retraceUtil (SetColour c Stop) acc pStyle pColour pPen = SetColour pColour acc
retraceUtil (PenDown Stop) acc pStyle pColour pPen = penState pPen acc
retraceUtil (PenUp Stop) acc pStyle pColour pPen = penState pPen acc
-- Recursive Cases
retraceUtil (Move d i) acc pStyle pColour pPen = retraceUtil i (Move (negate d) acc) pStyle  pColour pPen
retraceUtil (Turn a i) acc pStyle pColour pPen = retraceUtil i (Turn (negate a) acc) pStyle pColour pPen
retraceUtil (SetStyle l i) acc pStyle pColour pPen = retraceUtil i (SetStyle pStyle acc) l pColour pPen
retraceUtil (SetColour c i) acc pStyle pColour pPen = retraceUtil i (SetColour pColour acc) pStyle c pPen
retraceUtil (PenDown i) acc pStyle pColour pPen = retraceUtil i (penState pPen acc) pStyle pColour True
retraceUtil (PenUp i) acc pStyle pColour pPen = retraceUtil i (penState pPen acc) pStyle pColour False

-- Translate PenState bool to instruction
penState :: Bool -> Instructions -> Instructions
penState b i
  | b == True = PenDown i
  | otherwise = PenUp i

-----------------------------------------------------------------

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

