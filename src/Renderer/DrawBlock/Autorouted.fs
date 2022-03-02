module Autoroute

open BusWire

/// Extension work: 
/// Does not compile, and is not included in the rest of the code.
/// Only here for demonstration purposes.
let makeInitialSegmentsListFromRISegs connId (portCoords : XYPos * XYPos) : Segment list =
    // Coordinates of the ports
    let startPort: XYPos = snd(portCoords)
    let endPort: XYPos = fst(portCoords)

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let s = 
        let d = List.max [ abs (startPort.X - endPort.X) ; abs (startPort.Y - endPort.Y) ; Wire.stickLength / 4.0 ]
        if (endPort.X - startPort.X > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

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

    let buildRiSegListFromLengths (index:int) (length:float) = {
        Id = SegmentId(JSHelpers.uuid())
        Length= length
        HostId= connId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList= []
        Draggable = 
            if (index = 0 || index = lastIndex) then false else true
        Autorouted = true
    }

    let RISegs = lengthList |> List.mapi buildRiSegListFromLengths

    RISegs |> convertRISegsToSegments connId startPort wireRotation