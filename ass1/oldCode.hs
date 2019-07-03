
retrace :: Instructions -> Instructions
retrace (PenUp i) = retraceUtil i (Solid 1) white False
retrace i = retraceUtil i (Solid 1) white True

retraceUtil :: Instructions -> LineStyle -> Colour -> Bool -> Instructions
-- Base cases
retraceUtil (Stop) prevLineStyle prevColour penState = PenDown Stop
retraceUtil (Move d Stop) prevLineStyle prevColour penState = Move (negate d) Stop
retraceUtil (Turn a Stop) prevLineStyle prevColour penState = Turn (negate a) Stop
retraceUtil (SetStyle lineStyle Stop) prevLineStyle prevColour penState = SetStyle prevLineStyle Stop
retraceUtil (SetColour color Stop) prevLineStyle prevColour penState = SetColour prevColour Stop
retraceUtil (PenDown Stop) prevLineStyle prevColour penState = PenDown Stop
retraceUtil (PenUp Stop) prevLineStyle prevColour penState = PenDown Stop
-- Recursive cases
retraceUtil (Move d i) prevLineStyle prevColour penState = (retraceUtil i prevLineStyle prevColour penState) `andThen` (Move (negate d) Stop)
retraceUtil (Turn a i) prevLineStyle prevColour penState = (retraceUtil i prevLineStyle prevColour penState) `andThen` (Turn (negate a) Stop)
retraceUtil (SetStyle lineStyle i) prevLineStyle prevColour penState = (retraceUtil i lineStyle prevColour penState) `andThen` (SetStyle (prevLineStyle) Stop)
retraceUtil (SetColour color i) prevLineStyle prevColour penState = (retraceUtil i prevLineStyle color penState) `andThen` (SetColour (prevColour) Stop)
retraceUtil (PenDown i) prevLineStyle prevColour penState = (retraceUtil i prevLineStyle prevColour penState) `andThen` terminatePenState penState
retraceUtil (PenUp i) prevLineStyle prevColour penState = (retraceUtil i prevLineStyle prevColour penState) `andThen` terminatePenState penState


-----------------------------------------------------------------


retrace :: Instructions -> Instructions 
retrace i = retraceUtil i (SetStyle (Solid 1) $ SetColour white $ PenDown Stop)

retraceUtil :: Instructions -> Instructions -> Instructions
retraceUtil Stop acc = acc
retraceUtil (Move d Stop) acc = Move (negate d) acc
retraceUtil (Turn a Stop) acc = Turn (negate a) acc

retraceUtil (Move d i) acc = retraceUtil i (Move (negate d) acc)
retraceUtil (Turn a i) acc = retraceUtil i (Turn (negate a) acc)

-----------------------------------------------------------------

startStateInstructions :: Instructions
startStateInstructions = SetStyle (Solid 1) $ SetColour white $ PenDown Stop

retrace :: Instructions -> Instructions 
retrace i = retraceUtil i startStateInstructions (Solid 1) white

retraceUtil :: Instructions -> Instructions -> LineStyle -> Colour -> Instructions
retraceUtil Stop acc pStyle pColour = acc
retraceUtil (Move d Stop) acc pStyle pColour = Move (negate d) acc
retraceUtil (Turn a Stop) acc pStyle pColour = Turn (negate a) acc
retraceUtil (SetStyle l Stop) acc pStyle pColour = SetStyle pStyle acc
retraceUtil (SetColour c Stop) acc pStyle pColour = SetColour pColour acc

retraceUtil (Move d i) acc pStyle pColour = retraceUtil i (Move (negate d) acc) pStyle  pColour
retraceUtil (Turn a i) acc pStyle pColour = retraceUtil i (Turn (negate a) acc) pStyle pColour
retraceUtil (SetStyle l i) acc pStyle pColour = retraceUtil i (SetStyle pStyle acc) l pColour
retraceUtil (SetColour c i) acc pStyle pColour = retraceUtil i (SetColour pColour acc) pStyle c

-----------------------------------------------------------------
startStateInstructions :: Instructions
startStateInstructions = SetStyle (Solid 1) $ SetColour white $ PenDown Stop

retrace :: Instructions -> Instructions 
retrace (PenUp i) = retraceUtil i startStateInstructions (Solid 1) white False
retrace i = retraceUtil i startStateInstructions (Solid 1) white True

retraceUtil :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
retraceUtil Stop acc pStyle pColour pPen = acc
retraceUtil (Move d Stop) acc pStyle pColour pPen = Move (negate d) acc
retraceUtil (Turn a Stop) acc pStyle pColour pPen = Turn (negate a) acc
retraceUtil (SetStyle l Stop) acc pStyle pColour pPen = SetStyle pStyle acc
retraceUtil (SetColour c Stop) acc pStyle pColour pPen = SetColour pColour acc

retraceUtil (PenDown Stop) acc pStyle pColour pPen = PenDown acc
retraceUtil (PenUp Stop) acc pStyle pColour pPen = PenDown acc


retraceUtil (Move d i) acc pStyle pColour pPen = retraceUtil i (Move (negate d) acc) pStyle  pColour pPen
retraceUtil (Turn a i) acc pStyle pColour pPen = retraceUtil i (Turn (negate a) acc) pStyle pColour pPen
retraceUtil (SetStyle l i) acc pStyle pColour pPen = retraceUtil i (SetStyle pStyle acc) l pColour pPen
retraceUtil (SetColour c i) acc pStyle pColour pPen = retraceUtil i (SetColour pColour acc) pStyle c pPen

retraceUtil (PenDown i) acc pStyle pColour pPen = retraceUtil i (PenUp acc) pStyle pColour pPen
retraceUtil (PenUp i) acc pStyle pColour pPen = retraceUtil i (PenDown acc) pStyle pColour pPen

-----------------------------------------------------------------
-- import Debug.Trace
-- retraceUtil i acc pStyle pColour pPen | trace ("retraceUtil:" ++ show i) False = undefined
