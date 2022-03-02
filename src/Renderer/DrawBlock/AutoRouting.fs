module AutoRouting

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

let convertRISegsToSegments (hostId: ConnectionId)(startPos: XYPos) (startDir: int) (riSegs: BusWire.RotationInvariantSeg list) : BusWire.Segment list =
    
    let firstVector:XYPos = if (startDir = 90) || (startDir = 270) then {X=1;Y=0}
                            else {X=0;Y=1}                                                          //TODO: To check
    
    let firstSeg:BusWire.Segment = {
                            Id= SegmentId(JSHelpers.uuid())
                            Index = -1
                            Start = startPos
                            Vector = firstVector
                            HostId = hostId
                            JumpCoordinateList = []
                            Autorouted = true
                            Draggable = false
                            }

    let convertToSeg (oldState:BusWire.Segment) (element:BusWire.RotationInvariantSeg): BusWire.Segment  =
        // Current index of the segment
        let index = oldState.Index + 1
        // Compute the new start of the segment based on the old start + old vector
        let newStart = if (index = 0) then oldState.Start
                       else oldState.Start + oldState.Vector
        // Define the new vector based on orientation of the previous one
        let newBaseVector = if (oldState.Vector.X > 0)
                            then {X=0;Y=element.Length}
                            else {X=element.Length;Y=0}
        // Adjust the values of the vectors based on the current index and wire start orientation
        let newOrientedVector = match startDir with
                                | 0 -> newBaseVector
                                | 90 when ((index % 2) = 1) -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 180 -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 270 when ((index % 2) = 0) -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | _ -> newBaseVector
        
        let newDraggable = if(oldState.Index + 1 = 0) || (oldState.Index + 1 = riSegs.Length - 1) 
                           then false
                           else true 

        {
            Id = element.Id
            Index = oldState.Index + 1
            Start = newStart
            Vector = newOrientedVector
            HostId = hostId
            JumpCoordinateList = element.JumpCoordinateList
            Autorouted = true
            Draggable = newDraggable
        }
        

    let (segmentList: BusWire.Segment list) = ((firstSeg, riSegs) ||> List.scan convertToSeg)
    match segmentList with
    | hd::tl -> tl
    | _ -> []


let makeInitialSegmentsListFromRISegs connId (portCoords : XYPos * XYPos) : BusWire.Segment list =    //TODO: rename to makeInitialSegmentsListFromRISegs
    // Coordinates of the ports
    let startPort: XYPos = snd(portCoords)
    let endPort: XYPos = fst(portCoords)
    
    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let s = 
        let d = List.max [ abs (startPort.X - endPort.X) ; abs (startPort.Y - endPort.Y) ; BusWire.Wire.stickLength / 4.0 ]
        if (endPort.X - startPort.X > 0.0) then
            min d (BusWire.Wire.stickLength / 2.0)
        else
            BusWire.Wire.stickLength / 2.0

    // Rotation of the symbols - Constants for DEMO
    let startSymbolRotation = 0
    let endSymbolRotation = 0

    // Rotation of the ports
    let startPortRotation = startSymbolRotation
    let endPortRotation = (endSymbolRotation - 180) % 360

    // Overall rotation of the wire
    let wireRotation = startPortRotation

    let differenceInX, differenceInY = (endPort.X - startPort.X), (endPort.Y - startPort.Y) 

    // Get the NORMALIZED differences between the X and Y coordinates of the ports
    let diffX, diffY =
        match wireRotation with
        | 0 -> differenceInX, differenceInY
        | 90 -> - differenceInY, differenceInX
        | 180 -> - differenceInX, - differenceInY
        | 270 -> differenceInY, - differenceInX
        | _ -> differenceInY, differenceInX

    let normalizedEndPortRotation = startPortRotation - wireRotation

    let lengthList : float list = 
        match normalizedEndPortRotation with
        // Same orientation
        | 0 when (diffX >= 0) -> [s; 0; diffX; diffY; 0; 0; s]
        | 0 when (diffX < 0) -> [s; 0; 0; diffY; diffX; 0; s]
        // Opposite orientation
        | 180 when (diffX >= 0) -> [s; 0; (diffX - 2.0 * s)/2.0; diffY; (diffX - 2.0 * s)/2.0; 0; s]
        | 180 when (diffX < 0) -> [s; diffY/2.0; (diffX + 2.0 * s); diffY/2.0; 0; 0; s]
        // Perpendicular orientation: if startPort points to the right, endPort points down
        | 90 when ((diffX >= 0) && (diffY >= 0)) -> [s; 0; (diffX + s)/2.0; (diffY + s); (diffX + s)/2.0; 0; 0; s]
        | 90 when ((diffX >= 0) && (diffY < 0)) -> [s; 0; (diffX - s); (diffY - s); 0; 0; 0; s]
        | 90 when ((diffX < 0) && (diffY >= 0)) -> [s; 0; 0; (diffY + s); (diffX + s); 0; 0; s]
        | 90 when ((diffX < 0) && (diffY < 0)) -> [s; 0; 0; (diffY-s)/2.0; (diffX+s); (diffY-s)/2.0; 0; s]
        // Perpendicular orientation: if startPort points to the right, endPort points up
        | 270 when ((diffX >= 0) && (diffY >= 0)) -> [s; 0; (diffX - s); (diffY - s); 0; 0; 0; s]
        | 270 when ((diffX >= 0) && (diffY < 0)) -> [s; 0; (diffX - s)/2.0; (diffY + s); (diffX - s)/2.0; 0; 0; s]
        | 270 when ((diffX < 0) && (diffY >= 0)) -> [s; 0; 0; (diffY - s)/2.0; (diffX + s); (diffY - s)/2.0; 0; s]
        | 270 when ((diffX < 0) && (diffY < 0)) -> [s; 0; 0; (diffY + s); (diffX + s); 0; 0; s]
        // Edge case that should never happen
        | _ -> [s; 0; 0; 0; 0; 0; s]

    let lastIndex = (lengthList.Length - 1)

    let buildRiSegFromLength (index:int) (length:float): BusWire.RotationInvariantSeg = {
        Id = SegmentId(JSHelpers.uuid())
        Length= length
        HostId= connId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList= []
        Draggable = 
            if (index = 0 || index = lastIndex) then false else true
        Autorouted = true
    }
    
    let RISegs = lengthList |> List.mapi buildRiSegFromLength        //TODO: implement 'buildRiSegList' and 'riSegListToASegList'

    RISegs |> convertRISegsToSegments connId startPort wireRotation